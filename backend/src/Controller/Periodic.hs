{-# LANGUAGE OverloadedStrings #-}
module Controller.Periodic
  ( periodicHandler
    -- viewPeriodic
  , postPeriodic
  ) where

import qualified Data.ByteString as BS
import Control.Exception (throwTo)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Method (methodGet, methodPost)
import qualified Network.HTTP.Types.Method as HTM

import Has
import Model.Periodic
import Service.Types
import Action
import HTTP.Form.Deform
import HTTP.Path.Parser
import Controller.Permission
import Controller.Form
import View.Periodic

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

data RunPeriodicRequest = RunPeriodicRequest Bool

postPeriodicHandler :: Action
postPeriodicHandler = withAuth $ do
  checkMemberADMIN
  t <- peeks servicePeriodic
  RunPeriodicRequest w <- runForm (Just htmlPeriodic) (RunPeriodicRequest <$> ("weekly" .:> deform))
  liftIO $ mapM_ (`throwTo` if w then PeriodWeekly else PeriodDaily) t
  return $ okResponse [] (maybe "no" (const "ok") t :: String)

