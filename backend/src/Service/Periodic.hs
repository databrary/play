{-# LANGUAGE TupleSections, Rank2Types, ScopedTypeVariables #-}
module Service.Periodic
  ( forkPeriodic
  ) where

import Control.Concurrent (ThreadId, forkFinally, threadDelay)
import Control.Exception (handle, mask)
import Control.Monad (void, when)
import Control.Monad.Trans.Reader (withReaderT)
import Data.Fixed (Fixed(..), Micro)
import Data.IORef (writeIORef)
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import Data.Time.Clock (UTCTime(..), diffUTCTime, getCurrentTime)
import Data.Time.LocalTime (TimeOfDay(TimeOfDay), timeOfDayToTime)

import Has
import Service.Types
import Service.Log
import Service.Notification
import Context
import Model.Periodic
import Model.Token
import Model.Volume
import Model.Stats
import Model.Notification
import Controller.Notification
import Solr.Index
import EZID.Volume -- TODO

threadDelay' :: Micro -> IO ()
threadDelay' (MkFixed t)
  | t > m' = threadDelay m >> threadDelay' (MkFixed (t - m'))
  | otherwise = threadDelay (fromInteger t)
  where
  m' = toInteger m
  m = maxBound

run :: Period -> Service -> IO ()
run p = runContextM $ withReaderT BackgroundContext $ do
  t <- peek
  focusIO $ logMsg t ("periodic running: " ++ show p)
  cleanTokens
  updateVolumeIndex
  withReaderT (mkSolrIndexingContext . backgroundContext) updateIndex
  ss <- lookupSiteStats
  focusIO $ (`writeIORef` ss) . serviceStats
  when (p >= PeriodWeekly) $
    void updateEZID
  _ <- cleanNotifications
  updateStateNotifications
  focusIO $ triggerNotifications (Just p)

runPeriodic :: Service -> (forall a . IO a -> IO a) -> IO ()
runPeriodic rc unmask = loop (if s <= st then d s else s) where
  st = serviceStartTime rc
  s = st{ utctDayTime = timeOfDayToTime $ TimeOfDay 7 0 0 }
  d t = t{ utctDay = succ (utctDay t) }
  loop t = do
    n <- getCurrentTime
    (t', p) <- handle (return . (t ,)) $ do
      unmask $ threadDelay' $ realToFrac $ diffUTCTime t n
      return (d t, if 0 == snd (sundayStartWeek (utctDay t))
        then PeriodWeekly
        else PeriodDaily)
    handle (\(_ :: Period) -> logMsg t "periodic interrupted" (view rc)) $
      unmask $ run p rc
    loop t'

forkPeriodic :: Service -> IO ThreadId
forkPeriodic rc = forkFinally (mask $ runPeriodic rc) $ \r -> do
  t <- getCurrentTime
  logMsg t ("periodic aborted: " ++ show r) (view rc)
