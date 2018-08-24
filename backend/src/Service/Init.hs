{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Service.Init
  ( withService
  ) where

import Control.Exception (bracket)
import Control.Monad (when, void)
import Data.IORef (newIORef)
import Data.Time.Clock (getCurrentTime)

import Ops
import qualified Store.Config as C
import Service.DB (initDB, finiDB, runDBM)
import Service.Entropy (initEntropy)
import HTTP.Client (initHTTPClient)
import Store.Service (initStorage, StorageLocationConfig (..))
import Store.AV (initAV)
import Service.Passwd (initPasswd)
import Service.Log (initLogs, finiLogs)
import Service.Mail (initMailer)
import Service.Messages (loadMessages)
import Web.Service (initWeb)
import Static.Service (initStatic)
import Ingest.Service (initIngest)
import Model.Stats
import Solr.Service (initSolr, finiSolr)
import EZID.Service (initEZID)
import Service.Notification
import Service.Periodic (forkPeriodic)
import Service.Types
import Controller.Notification (forkNotifier)
import Store.Transcoder (initTranscoder)
import Store.Types
    (Transcoder, TranscoderConfig (..), Storage)

-- | Initialize a Service from a Config
initService
    :: Bool -- ^ Run in foreground?
    -> C.Config
    -> IO Service
initService fg conf = do
  time <- getCurrentTime
  logs <- initLogs (conf C.! (if fg then "log" else "log.bg"))
  mailer <- pure initMailer
  entropy <- initEntropy
  passwd <- initPasswd
  messages <- loadMessages
  db <- initDB (conf C.! "db")
  storage <- initStorage_ (conf C.! "store")
  av <- initAV
  web <- initWeb
  httpc <- initHTTPClient
  static <- initStatic (conf C.! "static")
  solr <- initSolr fg (conf C.! "solr")
  ezid <- initEZID (conf C.! "ezid")
  ingest <- initIngest
  notify <- initNotifications (conf C.! "notification")
  stats <- if fg then runDBM db lookupSiteStats else return (error "siteStats")
  statsref <- newIORef stats
  let rc = Service
        { serviceStartTime = time
        , serviceSecret = Secret $ conf C.! "secret"
        , serviceEntropy = entropy
        , servicePasswd = passwd
        , serviceLogs = logs
        , serviceMailer = mailer
        , serviceMessages = messages
        , serviceDB = db
        , serviceStorage = storage
        , serviceAV = av
        , serviceWeb = web
        , serviceHTTPClient = httpc
        , serviceStatic = static
        , serviceStats = statsref
        , serviceIngest = ingest
        , serviceSolr = solr
        , serviceEZID = ezid
        , servicePeriodic = Nothing
        , serviceNotification = notify
        , serviceDown = conf C.! "store.DOWN"
        }
  periodic <- fg `thenReturn` forkPeriodic rc
  when fg $ void $ forkNotifier rc
  return $! rc
    { servicePeriodic = periodic
    }
  where
  initTranscoder_ :: C.Config -> IO (Maybe Transcoder)
  initTranscoder_ c = initTranscoder TranscoderConfig
      { transcoderHost = c C.! "host"
      , transcoderDir = c C.! "dir"
      , transcoderMount = c C.! "mount"
      }
  initStorage_ :: C.Config -> IO Storage
  initStorage_ = initStorage . mkLocConf <*> initTranscoder_ . (C.! "transcode")
  mkLocConf :: C.Config -> Either String StorageLocationConfig
  mkLocConf c
      | Just down <- c C.! "DOWN" = Left down
      | otherwise = Right StorageLocationConfig
          { storageLocTemp = c C.! "temp"
          , storageLocMaster = c C.! "master"
          , storageLocUpload = c C.! "upload"
          , storageLocCache = c C.! "cache"
          , storageLocStage = c C.! "stage"
          , storageLocFallback = c C.! "fallback"
          }

-- | Close up Solr, database, and logs
finiService :: Service -> IO ()
finiService Service{..} = do
  finiSolr serviceSolr
  finiDB serviceDB
  finiLogs serviceLogs

-- | Bracket an action that uses a Service, governed by a Config.
withService
    :: Bool -- ^ Run in foreground?
    -> C.Config -- ^ Config for the Service
    -> (Service -> IO a) -- ^ Action to run
    -> IO a -- ^ Result of action
withService fg c = bracket (initService fg c) finiService
