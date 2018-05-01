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
import Control.Monad.Reader (ReaderT(..), withReaderT)
import Data.Time (getCurrentTime)
import Network.HTTP.Types (hDate, hCacheControl, methodHead)
import qualified Network.Wai as Wai

import Databrary.Has
import Databrary.HTTP
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

-- newtype Handler a = Handler { unHandler :: ReaderT RequestContext IO a }
-- withReaderT :: (r' -> r)-> ReaderT r m a-> ReaderT r' m a 

-- |
-- Given an action that runs in a Handler, build the necessary context for the
-- action, and run in it in the base-level Context
--
-- Handler has a richer context: Handler has all of Context, plus Identity and
-- the Wai Request.
withHandler
    :: forall a
     . Request -- ^ The wai request to handle
    -> Identity -- ^ The identity to use for this action
    -> Handler a -- ^ The action to perform
    -> ContextM a -- ^ The base-level control access to the system
withHandler waiReq identity h =
    let (handler :: ReaderT RequestContext IO a) = unHandler h
    in withReaderT (\(c :: Context) -> RequestContext c waiReq identity) handler

data NeedsAuth = NeedsAuth | DoesntNeedAuth

data Action = Action
  { _actionAuth :: !NeedsAuth
  , _actionM :: !(Handler Response)
  }

-- | FIXME: What does this function mean to us?
--
-- NB: This is the only place PreIdentified is used.
runAction :: Service -> Action -> Wai.Application
runAction service (Action needsAuth act) waiReq waiSend
    = let
          ident' = case needsAuth of
              NeedsAuth -> withHandler waiReq PreIdentified determineIdentity
              -- FIXME: Should this be NotIdentified?
              DoesntNeedAuth -> return PreIdentified
          fdaasdf = do
              identity <- ident'
              waiResponse <-
                  ReaderT
                      -- runResult is IO Response -> IO Response
                      $ \ctx -> runResult $ runHandler
                            act
                            (RequestContext ctx waiReq identity)
              return (identity, waiResponse)
      in do
          ts <- getCurrentTime
          (ident, waiResponse) <- runContextM fdaasdf service
          logAccess
              ts
              waiReq
              (foldIdentity
                  Nothing
                  (Just . (show :: Id Party -> String) . view)
                  ident
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
        . withReaderT (\a -> a { requestIdentity = ReIdentified u })
        . unHandler
