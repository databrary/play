{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TupleSections, Rank2Types, ScopedTypeVariables #-}
module Databrary.Service.Periodic
  ( forkPeriodic
  , forkZipGenerate
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

import Databrary.Has
import Databrary.Service.Types
import Databrary.Service.Log
import Databrary.Service.Notification
import Databrary.Context
import Databrary.Model.Periodic
import Databrary.Model.Token
import Databrary.Model.Volume
import Databrary.Model.Stats
import Databrary.Model.Notification
import Databrary.Controller.Notification
import Databrary.Solr.Index
import Databrary.EZID.Volume -- TODO

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
  updateIndex
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

-- TODO: move to separate module
runOnce :: Service -> IO ()
runOnce = runContextM $ withReaderT BackgroundContext $ do
  t <- peek
  focusIO $ logMsg t ("zip gen running")
  -- ss <- lookupSiteStats
--    for each entry
--        if Gen -> run generate and update with handle, (+ vol id, generated date)
--        if Ready -> check generated date; if old then delete entry
--    swap in new map
  -- focusIO $ (`writeIORef` ss) . serviceStats

runZipGenerate :: Service -> (forall a . IO a -> IO a) -> IO ()
runZipGenerate rc unmask = loop where
  loop = do
    t <- getCurrentTime
    _ <- -- handle (\_ -> return ()) $ do
      -- unmask $
      threadDelay' $ realToFrac $ 2
      -- return ()
    handle (\(_ :: Period) -> logMsg t "zip generate interrupted" (view rc)) $
      unmask $ runOnce rc
    -- loop

forkZipGenerate :: Service -> IO ThreadId
forkZipGenerate rc = forkFinally (mask $ runZipGenerate rc) $ \r -> do
  t <- getCurrentTime
  logMsg t ("zip generate aborted: " ++ show r) (view rc)
