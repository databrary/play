{-# LANGUAGE OverloadedStrings #-}
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
-- import Databrary.Controller.Analytics

withActionM :: Request -> Identity -> ActionM a -> ContextM a
withActionM r i = withReaderT (\c -> RequestContext c r i) . unActionM

data Action = Action
  { _actionAuth :: !Bool
  , _actionM :: !(ActionM Response)
  }

-- | FIXME: What does this function mean to us?
--
-- NB: This is the only place PreIdentified is used.
runAction :: Service -> Action -> Wai.Application
runAction rc (Action needsAuth act) req send = do
  ts <- getCurrentTime
  (i, r) <- runContextM (do
    (identity :: Identity) <- if needsAuth then withActionM req PreIdentified determineIdentity else return PreIdentified
    r <- ReaderT $ \ctx -> runResult $ runActionM (act) (RequestContext ctx req identity)
    return (identity, r))
    rc
  logAccess ts req (foldIdentity Nothing (Just . (show :: Id Party -> String) . view) i) r (serviceLogs rc)
  let isdb = isDatabraryClient req
      r' = Wai.mapResponseHeaders (((hDate, formatHTTPTimestamp ts) :) . (if isdb then ((hCacheControl, "no-cache") :) else id)) r
  send $ if Wai.requestMethod req == methodHead
    then emptyResponse (Wai.responseStatus r') (Wai.responseHeaders r')
    else r'

forkAction :: ActionM a -> RequestContext -> (Either SomeException a -> IO ()) -> IO ThreadId
forkAction f (RequestContext c r i) = forkFinally $
  runContextM (withActionM r i f) (contextService c)

withAuth :: ActionM Response -> Action
withAuth = Action True

withoutAuth :: ActionM Response -> Action
withoutAuth = Action False

-- | This may be like a 'su' that allows running an action as a different
-- SiteAuth.
--
-- FIXME: It is a little annoying that Identified carries a session, while
-- ReIdentified carries a SiteAuth.
withReAuth :: SiteAuth -> ActionM a -> ActionM a
withReAuth u =
    ActionM
        . withReaderT (\a -> a { requestIdentity = ReIdentified u })
        . unActionM
