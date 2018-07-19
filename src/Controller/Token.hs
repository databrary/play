{-# LANGUAGE CPP, OverloadedStrings #-}
module Controller.Token
  ( lookupPasswordResetAccount
  , viewLoginToken
  , postPasswordToken
  ) where

#if !defined(DEVEL) && !defined(SANDBOX)
import Control.Monad (mfilter)
#endif
import Control.Monad (when, unless)
import qualified Data.ByteString as BS
import Data.Maybe (isNothing, isJust)

import Ops
import Has
import qualified JSON
import Model.Id
import Model.Token
import Model.Party
#if !defined(DEVEL) && !defined(SANDBOX)
import Model.Permission
#endif
import Model.Notification.Types
import HTTP.Path.Parser
import Action.Run
import Action
import Controller.Paths
import Controller.Form
import Controller.Login
import Controller.Angular
import Controller.Notification
import View.Token

lookupPasswordResetAccount :: BS.ByteString -> Handler (Maybe SiteAuth)
lookupPasswordResetAccount email =
#if !defined(DEVEL) && !defined(SANDBOX)
  mfilter ((PermissionADMIN >) . accessMember) <$>
#endif
  lookupSiteAuthByEmail True email

viewLoginToken :: ActionRoute (API, Id LoginToken)
viewLoginToken = action GET (pathAPI </> pathId) $ \(api, ti) -> withoutAuth $ do
  when (api == HTML) angular
  tok <- maybeAction =<< lookupLoginToken ti
  if loginPasswordToken tok
    then case api of
      JSON -> return $ okResponse [] $ JSON.recordEncoding $ JSON.Record ti $
        "reset" JSON..= isJust (accountPasswd (view tok))
      HTML -> peeks $ blankForm . htmlPasswordToken ti
    else do
      _ <- removeLoginToken tok
      loginAccount api (view tok) False

postPasswordToken :: ActionRoute (API, Id LoginToken)
postPasswordToken = action POST (pathAPI </> pathId) $ \(api, ti) -> withoutAuth $ do
  tok <- maybeAction =<< lookupLoginToken ti
  unless (loginPasswordToken tok) $ result =<< peeks notFoundResponse
  let auth :: SiteAuth
      auth = view tok
  pw <- runForm ((api == HTML) `thenUse` htmlPasswordToken ti) $
    passwordForm (siteAccount auth)
  changeAccount auth{ accountPasswd = Just pw } -- or should this be withAuth?
  _ <- removeLoginToken tok
  unless (isNothing $ accountPasswd auth) $
    createNotification (blankNotification (siteAccount auth) NoticeAccountChange)
      { notificationParty = Just $ partyRow $ accountParty $ siteAccount auth }
  loginAccount api (view tok) False
