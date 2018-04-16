{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TupleSections, Rank2Types, ScopedTypeVariables #-}
module Databrary.Service.Periodic
  ( forkPeriodic
  , forkZipGenerate
  , insertGeneratingToken
  , updateWithCompletedHandle
  , getTokenValue
  ) where

import Control.Concurrent (ThreadId, forkFinally, threadDelay)
import Control.Exception (handle, mask)
import Control.Monad (void, when)
import Control.Monad.Trans.Reader (withReaderT)
import Data.Fixed (Fixed(..), Micro)
import Data.IORef (writeIORef, modifyIORef', readIORef)
import qualified Data.Map as Map
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import Data.Time.Clock (UTCTime(..), diffUTCTime, getCurrentTime)
import Data.Time.LocalTime (TimeOfDay(TimeOfDay), timeOfDayToTime)
import System.IO (Handle)

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
  focusIO $ logMsg t ("zip gen running garbage collect")
  -- ss <- lookupSiteStats
--    for each entry
--        if Ready -> check generated date; if old then delete entry
--    swap in new map
  -- focusIO $ (`writeIORef` ss) . serviceStats

runZipGenerate :: Service -> (forall a . IO a -> IO a) -> IO ()
runZipGenerate rc unmask = loop where
  loop = do
    t <- getCurrentTime
    _ <- -- handle (\_ -> return ()) $ do
      -- unmask $
      threadDelay' $ realToFrac $ 15
      -- return ()
    handle (\(_ :: Period) -> logMsg t "zip generate collect interrupt" (view rc)) $
      unmask $ runOnce rc
    -- loop

forkZipGenerate :: Service -> IO ThreadId
forkZipGenerate rc = forkFinally (mask $ runZipGenerate rc) $ \r -> do
  t <- getCurrentTime
  logMsg t ("zip generate aborted: " ++ show r) (view rc)

insertGeneratingToken :: String -> Service -> IO ()
insertGeneratingToken tkn = runContextM $ withReaderT BackgroundContext $ do
  focusIO $ (\mpRef -> modifyIORef' mpRef (\mp -> Map.insert tkn Nothing mp)) . serviceZipGenerate
  pure ()

updateWithCompletedHandle :: String -> Handle -> Integer -> Service -> IO ()
updateWithCompletedHandle tkn zipHndl sz = runContextM $ withReaderT BackgroundContext $ do
  focusIO
    $ (\mpRef -> modifyIORef' mpRef (\mp -> Map.insert tkn (Just (zipHndl, sz)) mp)) . serviceZipGenerate
  pure ()

getTokenValue :: String -> Service -> IO (Maybe (Maybe (Handle, Integer))) -- TODO: remove token value
getTokenValue tkn = runContextM $ withReaderT BackgroundContext $ do
  mp <- focusIO $ (\mpRef -> readIORef mpRef)
  pure (Map.lookup tkn mp)
