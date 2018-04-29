{-# LANGUAGE TemplateHaskell #-}
module Databrary.Context
  ( Context(..)
  , ContextM
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

data Context = Context
  { contextService :: !Service
  , contextTimestamp :: !Timestamp
  , contextResourceState :: !InternalState
  , contextDB :: !DBConn
  }

-- makeHasRec ''Context ['contextService, 'contextTimestamp, 'contextResourceState, 'contextDB]
instance Has Service Context where
  view = contextService
instance Has Databrary.Service.Notification.Notifications Context where
   view = (view . contextService)
instance Has Databrary.Solr.Service.Solr Context where
  view = (view . contextService)
instance Has Databrary.Ingest.Service.Ingest Context where
  view = (view . contextService)
instance Has Databrary.Static.Service.Static Context where
  view = (view . contextService)
instance Has Databrary.HTTP.Client.HTTPClient Context where
  view = (view . contextService)
instance Has Databrary.Web.Types.Web Context where
  view = (view . contextService)
instance Has Databrary.Store.AV.AV Context where
  view = (view . contextService)
instance Has Databrary.Store.Types.Storage Context where
  view = (view . contextService)
-- instance Has DBPool Context where
--   view = (view . contextService)
instance Has Databrary.Service.Messages.Messages Context where
  view = (view . contextService)
instance Has Databrary.Service.Log.Logs Context where
  view = (view . contextService)
instance Has Databrary.Service.Passwd.Passwd Context where
  view = (view . contextService)
instance Has Databrary.Service.Entropy.Entropy Context where
  view = (view . contextService)
instance Has Secret Context where
  view = (view . contextService)
instance Has Timestamp Context where
  view = contextTimestamp
-- instance Has time-1.6.0.1:Data.Time.Calendar.Days.Day Context where
--   view = (view . contextTimestamp)
instance Has InternalState Context where
  view = contextResourceState
instance Has DBConn Context where
  view = contextDB

type ContextM a = ReaderT Context IO a

runContextM :: ContextM a -> Service -> IO a
runContextM f rc = do
  t <- getCurrentTime
  runResourceT $ withInternalState $ \is ->
    withDB (serviceDB rc) $
      runReaderT f . Context rc t is

newtype BackgroundContext = BackgroundContext { backgroundContext :: Context }

-- makeHasRec ''BackgroundContext ['backgroundContext]
-- instance Has Context BackgroundContext where
--   view = backgroundContext
instance Has Service BackgroundContext where
  view = (view . backgroundContext)
instance Has Databrary.Service.Notification.Notifications BackgroundContext where
  view = (view . backgroundContext)
instance Has Databrary.Solr.Service.Solr BackgroundContext where
  view = (view . backgroundContext)
instance Has Databrary.Ingest.Service.Ingest BackgroundContext where
  view = (view . backgroundContext)
-- instance Has Databrary.Static.Service.Static BackgroundContext where
--   view = (view . backgroundContext)
instance Has Databrary.HTTP.Client.HTTPClient BackgroundContext where
  view = (view . backgroundContext)
-- instance Has Databrary.Web.Types.Web BackgroundContext where
--   view = (view . backgroundContext)
-- instance Has Databrary.Store.AV.AV BackgroundContext where
--   view = (view . backgroundContext)
instance Has Databrary.Store.Types.Storage BackgroundContext where
  view = (view . backgroundContext)
-- instance Has DBPool BackgroundContext where
--   view = (view . backgroundContext)
-- instance Has Databrary.Service.Messages.Messages BackgroundContext where
--   view = (view . backgroundContext)
instance Has Databrary.Service.Log.Logs BackgroundContext where
  view = (view . backgroundContext)
-- instance Has Databrary.Service.Passwd.Passwd BackgroundContext where
--   view = (view . backgroundContext)
-- instance Has Databrary.Service.Entropy.Entropy BackgroundContext where
--   view = (view . backgroundContext)
-- instance Has Secret BackgroundContext where
--   view = (view . backgroundContext)
instance Has Timestamp BackgroundContext where
  view = (view . backgroundContext)
-- instance Has time-1.6.0.1:Data.Time.Calendar.Days.Day BackgroundContext where
--   view = (view . backgroundContext)
-- instance Has InternalState BackgroundContext where
--   view = (view . backgroundContext)
instance Has DBConn BackgroundContext where
  view = (view . backgroundContext)

instance Has Identity BackgroundContext where
  view _ = NotIdentified
instance Has SiteAuth BackgroundContext where
  view _ = view NotIdentified
instance Has Party BackgroundContext where
  view _ = view NotIdentified
instance Has (Id Party) BackgroundContext where
  view _ = view NotIdentified
instance Has Access BackgroundContext where
  view _ = view NotIdentified

type BackgroundContextM a = ReaderT BackgroundContext IO a

withBackgroundContextM :: BackgroundContextM a -> ContextM a
withBackgroundContextM = withReaderT BackgroundContext
