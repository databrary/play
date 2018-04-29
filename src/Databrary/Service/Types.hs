{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Service.Types
  ( Secret(..)
  , Service(..)
  ) where

import Control.Concurrent (ThreadId)
import qualified Data.ByteString as BS
import Data.IORef (IORef)
import qualified Data.Text as T

import Databrary.Has (Has(..))
import Databrary.Service.DB (DBPool)
import Databrary.Service.Entropy (Entropy)
import Databrary.HTTP.Client (HTTPClient)
import Databrary.Store.Types (Storage)
import Databrary.Store.AV (AV)
import Databrary.Service.Passwd (Passwd)
import Databrary.Service.Log (Logs)
import Databrary.Service.Messages (Messages)
import Databrary.Web.Types (Web)
import Databrary.Static.Service (Static)
import Databrary.Solr.Service (Solr)
import Databrary.Ingest.Service (Ingest)
import Databrary.EZID.Service (EZID)
import Databrary.Service.Notification (Notifications)
import Databrary.Model.Time
import Databrary.Model.Stats.Types

newtype Secret = Secret BS.ByteString

data Service = Service
  { serviceStartTime :: !Timestamp
  , serviceSecret :: !Secret
  , serviceEntropy :: !Entropy
  , servicePasswd :: !Passwd
  , serviceLogs :: !Logs
  , serviceMessages :: !Messages
  , serviceDB :: !DBPool
  , serviceStorage :: Storage -- may be down
  , serviceAV :: !AV
  , serviceWeb :: !Web
  , serviceHTTPClient :: !HTTPClient
  , serviceStatic :: !Static
  , serviceStats :: !(IORef SiteStats)
  , serviceIngest :: !Ingest
  , serviceSolr :: !Solr
  , serviceEZID :: !(Maybe EZID)
  , servicePeriodic :: !(Maybe ThreadId)
  , serviceNotification :: !Notifications
  , serviceDown :: !(Maybe T.Text)
  }

-- makeHasRec ''Service ['serviceSecret, 'serviceEntropy, 'servicePasswd, 'serviceLogs, 'serviceMessages, 'serviceDB, 'serviceStorage, 'serviceAV, 'serviceWeb, 'serviceHTTPClient, 'serviceStatic, 'serviceIngest, 'serviceSolr, 'serviceNotification]
instance Has Secret Service where
  view = serviceSecret
instance Has Entropy Service where
  view = serviceEntropy
instance Has Passwd Service where
  view = servicePasswd
instance Has Logs Service where
  view = serviceLogs
instance Has Messages Service where
  view = serviceMessages
-- instance Has DBPool Service where
--    view = serviceDB
instance Has Storage Service where
  view = serviceStorage
instance Has AV Service where
  view = serviceAV
instance Has Web Service where
  view = serviceWeb
instance Has HTTPClient Service where
  view = serviceHTTPClient
instance Has Static Service where
  view = serviceStatic
instance Has Ingest Service where
  view = serviceIngest
instance Has Solr Service where
  view = serviceSolr
instance Has Notifications Service where
  view = serviceNotification
