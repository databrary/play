{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Periodic
  ( periodicHandler
    -- viewPeriodic
  , postPeriodic
  ) where

import qualified Data.ByteString as BS
import Control.Exception (throwTo)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Method (methodGet, methodPost)
import qualified Network.HTTP.Types.Method as HTM

import Databrary.Has
import Databrary.Model.Periodic
import Databrary.Service.Types
import Databrary.Action
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.View.Periodic

periodicHandler :: HTM.Method -> [(BS.ByteString, BS.ByteString)] -> Action
periodicHandler method _
    | method == methodGet = viewPeriodicHandler
    | method == methodPost = postPeriodicHandler
    | otherwise = error "unhandled api/method combo" -- TODO: better error 

{-
viewPeriodic :: ActionRoute ()
viewPeriodic = action GET ("admin" >/> "periodic") $ \() -> withAuth $ do
  checkMemberADMIN
  peeks $ blankForm . htmlPeriodic
-}
viewPeriodicHandler :: Action
viewPeriodicHandler = withAuth $ do
  checkMemberADMIN
  peeks $ blankForm . htmlPeriodic

postPeriodic :: ActionRoute ()
postPeriodic = action POST ("admin" >/> "periodic") $ \() -> postPeriodicHandler

postPeriodicHandler :: Action
postPeriodicHandler = withAuth $ do
  checkMemberADMIN
  t <- peeks servicePeriodic
  w <- runForm (Just htmlPeriodic) $ "weekly" .:> deform
  liftIO $ mapM_ (`throwTo` if w then PeriodWeekly else PeriodDaily) t
  return $ okResponse [] (maybe "no" (const "ok") t :: String)

