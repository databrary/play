{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Databrary.Context
  ( ActionContext(..)
  , ActionContextM
  , runContextM
  , BackgroundContext(..)
  , BackgroundContextM
  , withBackgroundContextM
  ) where

import Control.Monad.Trans.Reader (ReaderT(..), withReaderT)
import Control.Monad.Trans.Resource (InternalState, runResourceT, withInternalState)
import Data.Time (getCurrentTime)

import Databrary.Has
import Databrary.HTTP.Client
import Databrary.Model.Time
import Databrary.Model.Id.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Party.Types
import Databrary.Model.Permission.Types
import Databrary.Service.Log
import Databrary.Service.Types
import Databrary.Service.DB
import Databrary.Service.Entropy
import Databrary.Service.Messages
import Databrary.Service.Notification
import Databrary.Service.Passwd
import Databrary.Solr.Service
import Databrary.Static.Service
import Databrary.Ingest.Service
import Databrary.Store.AV
import Databrary.Store.Types
import Databrary.Web.Types

-- | This is the context for when you don't have an identity, but you have a
-- fully initialized, "command line" access to the system.
data ActionContext = ActionContext
  { contextService :: !Service -- ^ All initialized services; "the imperative shell"
  , contextTimestamp :: !Timestamp -- ^ When the ActionContextM action is running (i.e., NOW)
  , contextResourceState :: !InternalState -- ^ Optimization for MonadResource
  , contextDB :: !DBConn -- ^ The specific connection chosen for the running action?
  }

instance Has Service ActionContext where
  view = contextService
instance Has Databrary.Service.Notification.Notifications ActionContext where
   view = (view . contextService)
instance Has Databrary.Solr.Service.Solr ActionContext where
  view = (view . contextService)
instance Has Databrary.Ingest.Service.Ingest ActionContext where
  view = (view . contextService)
instance Has Databrary.Static.Service.Static ActionContext where
  view = (view . contextService)
instance Has Databrary.HTTP.Client.HTTPClient ActionContext where
  view = (view . contextService)
instance Has Databrary.Web.Types.Web ActionContext where
  view = (view . contextService)
instance Has Databrary.Store.AV.AV ActionContext where
  view = (view . contextService)
instance Has Databrary.Store.Types.Storage ActionContext where
  view = (view . contextService)
instance Has Databrary.Service.Messages.Messages ActionContext where
  view = (view . contextService)
instance Has Databrary.Service.Log.Logs ActionContext where
  view = (view . contextService)
instance Has Databrary.Service.Passwd.Passwd ActionContext where
  view = (view . contextService)
instance Has Databrary.Service.Entropy.Entropy ActionContext where
  view = (view . contextService)
instance Has Secret ActionContext where
  view = (view . contextService)
instance Has InternalState ActionContext where
  view = contextResourceState
instance Has DBConn ActionContext where
  view = contextDB

type ActionContextM a = ReaderT ActionContext IO a

-- | Perform an atomic action without an identity with a guaranteed database
-- connection and a fixed version of 'now'.
runContextM
    :: ActionContextM a
    -> Service
    -> IO a
runContextM action rc = do
    t <- getCurrentTime
    runResourceT $ withInternalState $ \is ->
        withDB (serviceDB rc) $ runReaderT action . ActionContext rc t is

-- | A ActionContext with no Identity.
newtype BackgroundContext = BackgroundContext { backgroundContext :: ActionContext }
    deriving
        ( Has Service
        , Has Notifications
        , Has Solr
        , Has Ingest
        , Has HTTPClient
        , Has Storage
        , Has Logs
        , Has DBConn
        )

instance Has Timestamp BackgroundContext where
  view = (contextTimestamp . backgroundContext)
instance Has Identity BackgroundContext where
  view _ = IdentityNotNeeded
instance Has SiteAuth BackgroundContext where
  view _ = view IdentityNotNeeded
instance Has Party BackgroundContext where
  view _ = view IdentityNotNeeded
instance Has (Id Party) BackgroundContext where
  view _ = view IdentityNotNeeded
instance Has Access BackgroundContext where
  view _ = view IdentityNotNeeded

type BackgroundContextM a = ReaderT BackgroundContext IO a

withBackgroundContextM :: BackgroundContextM a -> ActionContextM a
withBackgroundContextM = withReaderT BackgroundContext
