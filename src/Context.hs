{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Context
  ( ActionContext(..)
  , ActionContextM
  , runContextM
  , BackgroundContext(..)
  , BackgroundContextM
  , withBackgroundContextM
  , SolrIndexingContext(..)
  , SolrIndexingContextM
  , mkSolrIndexingContext
  ) where

import Control.Monad.Trans.Reader (ReaderT(..), withReaderT)
import Control.Monad.Trans.Resource (InternalState, runResourceT, withInternalState)
import Data.Time (getCurrentTime)

import Has
import HTTP.Client
import Model.Time
import Model.Id.Types
import Model.Identity.Types
import Model.Party.Types
import Model.Permission.Types
import Service.Log
import Service.Types
import Service.DB
import Service.Entropy
import Service.Mail
import Service.Messages
import Service.Notification
import Service.Passwd
import Solr.Service
import Static.Service
import Ingest.Service
import Store.AV
import Store.Types
import Web.Types

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
instance Has Service.Notification.Notifications ActionContext where
   view = view . contextService
instance Has Solr.Service.Solr ActionContext where
  view = view . contextService
instance Has Ingest.Service.Ingest ActionContext where
  view = view . contextService
instance Has Static.Service.Static ActionContext where
  view = view . contextService
instance Has HTTP.Client.HTTPClient ActionContext where
  view = view . contextService
instance Has Web.Types.Web ActionContext where
  view = view . contextService
instance Has Store.AV.AV ActionContext where
  view = view . contextService
instance Has Store.Types.Storage ActionContext where
  view = view . contextService
instance Has Service.Messages.Messages ActionContext where
  view = view . contextService
instance Has Service.Log.Logs ActionContext where
  view = view . contextService
instance Has Service.Mail.Mailer ActionContext where
  view = serviceMailer . contextService
instance Has Service.Passwd.Passwd ActionContext where
  view = view . contextService
instance Has Service.Entropy.Entropy ActionContext where
  view = view . contextService
instance Has Secret ActionContext where
  view = view . contextService
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
  view = contextTimestamp . backgroundContext
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

-- | A ActionContext with no Identity, for running Solr indexing.
data SolrIndexingContext = SolrIndexingContext
  { slcLogs :: !Logs
  , slcHTTPClient :: !HTTPClient
  , slcSolr :: !Solr
  , slcDB :: !DBConn -- ^ The specific connection chosen for the running action?
  }

instance Has Solr SolrIndexingContext where
  view = slcSolr
instance Has Logs SolrIndexingContext where
  view = slcLogs
instance Has HTTPClient SolrIndexingContext where
  view = slcHTTPClient
instance Has DBConn SolrIndexingContext where
  view = slcDB

instance Has Identity SolrIndexingContext where
  view _ = IdentityNotNeeded
instance Has SiteAuth SolrIndexingContext where
  view _ = view IdentityNotNeeded
instance Has Party SolrIndexingContext where
  view _ = view IdentityNotNeeded
instance Has (Id Party) SolrIndexingContext where
  view _ = view IdentityNotNeeded
instance Has Access SolrIndexingContext where
  view _ = view IdentityNotNeeded

type SolrIndexingContextM a = ReaderT SolrIndexingContext IO a

-- | Build a simpler SolrIndexingContext from a complete ActionContext
mkSolrIndexingContext :: ActionContext -> SolrIndexingContext
mkSolrIndexingContext ac =
    SolrIndexingContext {
          slcLogs = (serviceLogs . contextService) ac
        , slcHTTPClient = (serviceHTTPClient . contextService) ac
        , slcSolr = (serviceSolr . contextService) ac
        , slcDB = contextDB ac
    }
