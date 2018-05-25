{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- How to run Databrary Actions.
--
-- This module is the foundation of the site. It provides a method for packaging
-- up route actions into 'Wai.Application's (which is effectively the HTTP
-- transport layer), as well as a method for packaging them into background
-- threads.
module Databrary.Action.Run
  (
  -- * \"Framework\" code
    actionApp
  , forkAction
  -- * Declaring authentication needs for Actions
  , withAuth
  , withoutAuth
  , withReAuth
  -- * The underlying type
  , Action
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

-- | Authentication requirements for an 'Action'.
data NeedsAuthentication = NeedsAuthentication | DoesntNeedAuthentication

-- | This type captures both the authentication needs and the handler for a
-- route.
--
-- This type is only found buried within an 'ActionRoute'.
data Action = Action
    { _actionAuthentication :: !NeedsAuthentication
    , _actionM :: !(Handler Response)
    }

-- | Special-purpose context for determing the user's identity. We don't need
-- the full Handler for that, and since this is sensitive work, we don't want to
-- just cheat and use it anyway.
data IdContext = IdContext
    { ctxReq :: Wai.Request
    , ctxSec :: Secret
    , ctxConn :: DBConn
    }

instance Has Wai.Request IdContext where view = ctxReq
instance Has Secret IdContext where view = ctxSec
instance Has DBConn IdContext where view = ctxConn


-- | Convert an Action into a Wai.Application.
--
-- This is the 2nd-lowest level of the Databrary \"web framework\". It runs the
-- requested action with some resolved identity to build the HTTP response. See
-- 'actionRouteApp' for level 1.
actionApp
    :: Service -- ^ All the low-level system capabilities
    -> Action -- ^ Action to run
    -> Wai.Application -- ^ Callback for Wai
actionApp service (Action needsAuth act) waiReq waiSend
    = let
          ident' :: Secret -> DBConn -> ReaderT ActionContext IO Identity
          ident' sec con = case needsAuth of
              NeedsAuthentication -> runReaderT determineIdentity (IdContext waiReq sec con)
              DoesntNeedAuthentication -> return IdentityNotNeeded
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

-- | Run a Handler action in the background (IO).
--
-- A new ContextM is built, with a fresh guaranteed db connection and timestamp,
-- via 'runContextM'
forkAction
    :: Handler a -- ^ Handler action to run
    -> RequestContext
    -- ^ Original context
    --
    -- The background action inherits this context's 'Request' and 'Service'.
    -> (Either SomeException a -> IO ())
    -- ^ Cleanup to run when the action exits
    -> IO ThreadId
forkAction h reqCtx = forkFinally $ runContextM (withHandler req ident h) srv
  where
    req = contextRequest reqCtx
    ident = requestIdentity reqCtx
    srv = contextService (requestContext reqCtx)

-- | Tag an 'Action' as needing authentication.
--
-- For reference, note that Yesod's equivalent to this function returns
-- @Handler Identity@, and throws a permission error when the user isn't
-- identified.
withAuth
    :: Handler Response -- ^ The handler for the route
    -> Action -- ^ The bundled action
withAuth = Action NeedsAuthentication

-- | Tag an 'Action' as not needing auth. See note at 'withAuth'.
withoutAuth :: Handler Response -> Action
withoutAuth = Action DoesntNeedAuthentication

-- | This may be like a 'su' that allows running an action as a different
-- identity.
withReAuth
    :: SiteAuth -- ^ The identity to assume
    -> Handler a
    -- ^ The action to perform as the assumed identity
    --
    -- Note that one might argue for an indexed form of 'Handler' here, a la
    -- Selda and subqueries.
    -> Handler a -- ^ The re-authenticated action, packaged into the original context
withReAuth u =
    Handler
        . local (\a -> a { requestIdentity = ReIdentified u })
        . unHandler
