{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Databrary.Action.Run
  ( Action
  , runAction
  , forkAction
  , withAuth
  , withoutAuth
  , withReAuth
  ) where

import Control.Concurrent (ThreadId, forkFinally)
import Control.Exception (SomeException)
import Control.Monad.Reader (ReaderT(..), withReaderT, local)
import Data.Time (getCurrentTime)
import Network.HTTP.Types (hDate, hCacheControl, methodHead)
import qualified Network.Wai as Wai

import Databrary.Has
import Databrary.HTTP
import Databrary.Service.DB
import Databrary.Service.Types
import Databrary.Service.Log
import Databrary.Model.Identity
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.HTTP.Request
import Databrary.Context
import Databrary.Action.Types
import Databrary.Action.Request
import Databrary.Action.Response

-- |
-- Transform a web request to a lower-level action.
--
-- Given an action that runs in a top-level Handler, build the necessary context
-- for that action, and run in it in the base-level ContextM
--
-- Handler has a richer context: it has all of ActionContext, plus Identity and
-- the Wai Request.
withHandler
    :: forall a
     . Request -- ^ The wai request to handle
    -> Identity -- ^ The identity to use for this action
    -> Handler a -- ^ The action to perform
    -> ContextM a -- ^ The base-level control access to the system
withHandler waiReq identity h =
    let (handler :: ReaderT RequestContext IO a) = unHandler h
    in withReaderT (\(c :: ActionContext) -> RequestContext c waiReq identity) handler

data NeedsAuth = NeedsAuth | DoesntNeedAuth

data Action = Action
  { _actionAuth :: !NeedsAuth
  , _actionM :: !(Handler Response)
  }

-- | Special-purpose context for determing the user's identity. We don't need
-- the full Handler for that, and since this is sensitive work, we don't want to
-- just cheat and use it anyway.
data IdContext = IdContext
    { ctxReq :: Wai.Request
    , ctxSec :: Secret
    , ctxConn :: DBConn }

instance Has Wai.Request IdContext where view = ctxReq
instance Has Secret IdContext where view = ctxSec
instance Has DBConn IdContext where view = ctxConn

-- | Run the requested action with some resolved identity,
-- send response, with some header decorations.
runAction :: Service -> Action -> Wai.Application
runAction service (Action needsAuth act) waiReq waiSend
    = let
          ident' :: Secret -> DBConn -> ReaderT ActionContext IO Identity
          ident' sec con = case needsAuth of
              NeedsAuth -> runReaderT determineIdentity (IdContext waiReq sec con)
              DoesntNeedAuth -> return IdentityNotNeeded
          authenticatedAct :: ContextM (Identity, Response)
          authenticatedAct = do
              sec <- peek
              conn <- peek
              identity <- ident' sec conn
              waiResponse <-
                  ReaderT
                      -- runResult is IO Response -> IO Response
                      $ \ctx -> runResult $ runHandler
                            act
                            (RequestContext ctx waiReq identity)
              return (identity, waiResponse)
      in do
          ts <- getCurrentTime
          (identityUsed, waiResponse) <- runContextM authenticatedAct service
          logAccess
              ts
              waiReq
              (extractFromIdentifiedSessOrDefault
                  Nothing
                  (Just . (show :: Id Party -> String) . view)
                  identityUsed
              )
              waiResponse
              (serviceLogs service)
          let
              isdb = isDatabraryClient waiReq
              waiResponse' = Wai.mapResponseHeaders
                  (((hDate, formatHTTPTimestamp ts) :)
                  . (if isdb then ((hCacheControl, "no-cache") :) else id)
                  )
                  waiResponse
          waiSend $ if Wai.requestMethod waiReq == methodHead
              then emptyResponse
                  (Wai.responseStatus waiResponse')
                  (Wai.responseHeaders waiResponse')
              else waiResponse'

forkAction :: Handler a -> RequestContext -> (Either SomeException a -> IO ()) -> IO ThreadId
forkAction f (RequestContext c r i) = forkFinally $
  runContextM (withHandler r i f) (contextService c)

withAuth :: Handler Response -> Action
withAuth = Action NeedsAuth

withoutAuth :: Handler Response -> Action
withoutAuth = Action DoesntNeedAuth

-- | This may be like a 'su' that allows running an action as a different
-- SiteAuth.
--
-- FIXME: It is a little annoying that Identified carries a session, while
-- ReIdentified carries a SiteAuth.
withReAuth :: SiteAuth -> Handler a -> Handler a
withReAuth u =
    Handler
        . local (\a -> a { requestIdentity = ReIdentified u })
        . unHandler
