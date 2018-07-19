{-# LANGUAGE OverloadedStrings #-}
module Controller.Login
  ( checkPassword
  , loginAccount
  , viewLogin
  , postLogin
  , loginHandler
  , postLogout
  , postLogoutHandler
  , postUser
  , userHandler
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import qualified Crypto.BCrypt as BCrypt
import qualified Data.ByteString as BS
import Data.Function (on)
import Data.Maybe (fromMaybe)
import qualified Network.Wai as Wai
import Network.HTTP.Types.Method (methodGet, methodPost)
import qualified Network.HTTP.Types.Method as HTM

import Ops
import Has
import qualified JSON
import Model.Id.Types
import Model.Party
import Model.Identity
import Model.Permission
import Model.Notification
import Model.Token
import HTTP.Cookie
import HTTP.Form.Deform
import HTTP.Path.Parser
import Action
import Controller.Paths
import Controller.Form
import Controller.Angular
import Controller.Notification
import View.Login

import {-# SOURCE #-} Controller.Root
import {-# SOURCE #-} Controller.Party

loginAccount :: API -> SiteAuth -> Bool -> Handler Response
loginAccount api auth su = do
  (sess :: Session) <- createSession auth su
  let Token (Id tok) ex = (accountToken . sessionAccountToken) sess
  cook <- setSignedCookie "session" tok ex
  case api of
    JSON -> return $ okResponse [cook] $ JSON.recordEncoding $ identityJSON (Identified sess)
    HTML -> peeks $ otherRouteResponse [cook] viewParty (HTML, TargetProfile)

loginHandler :: API -> HTM.Method -> [(BS.ByteString, BS.ByteString)] -> Action
loginHandler api method _
    | method == methodGet && api == HTML = viewLoginAction
    | method == methodPost = postLoginAction api
    | otherwise = error "unhandled api/method combo" -- TODO: better error

viewLogin :: ActionRoute ()
viewLogin = action GET ("user" >/> "login") $ \() -> viewLoginAction

-- | The action for handling GET for user/login
viewLoginAction :: Action
viewLoginAction = withAuth $ do
    angular
    maybeIdentity
        (peeks (blankForm . htmlLogin))
        (const (peeks (otherRouteResponse [] viewParty (HTML, TargetProfile))))

checkPassword :: BS.ByteString -> SiteAuth -> Bool
checkPassword p = any (`BCrypt.validatePassword` p) . accountPasswd

postLogin :: ActionRoute API
postLogin = action POST (pathAPI </< "user" </< "login") $ postLoginAction

data LoginRequest = LoginRequest BS.ByteString BS.ByteString Bool

-- | The action for handling POST for user/login
postLoginAction :: API -> Action
postLoginAction = \api -> withoutAuth $ do
  (Just auth, su) <- runForm ((api == HTML) `thenUse` htmlLogin) $ do
    email <- "email" .:> emailTextForm
    password <- "password" .:> deform
    superuser <- "superuser" .:> deform
    let _ = LoginRequest email password superuser
    (auth :: Maybe SiteAuth) <- lift $ lookupSiteAuthByEmail True email
    let p :: Maybe Party
        p = view <$> auth
        -- The site auth will contain a member value, indicating the
        --  user's right to edit group 0 (databrary site). There is no
        --  inheritance for this value, so this is essentially looking
        --  at the member value for the direct authorization between the user and
        --  group 0. See examples of typical superadmins like party 1, party 7 in 0.sql.
        su = superuser && any ((PermissionADMIN ==) . accessMember) auth
    attempts <- lift $ maybe (return 0) recentAccountLogins p
    let pass = checkPassword password `any` auth
        block = attempts > 4
    lift $ auditAccountLogin pass (fromMaybe nobodyParty p) email
    when block $ "email" .:> deformError "Too many login attempts. Try again later."
    unless pass $ "password" .:> deformError "Incorrect email address or password. Both are case-sensitive, and institutional addresses are preferred."
    return (auth, su)
  loginAccount api auth su

postLogout :: ActionRoute API
postLogout = action POST (pathAPI </< "user" </< "logout") $ \api -> postLogoutHandler api []

postLogoutHandler :: API -> [(BS.ByteString, BS.ByteString)] -> Action  -- TODO: guard against methods besides POST
postLogoutHandler = \api _ -> withAuth $ do
  _ <- maybeIdentity (return False) removeSession
  case api of
    JSON -> return $ okResponse [cook] $ JSON.recordEncoding $ identityJSON NotLoggedIn
    HTML -> peeks $ otherRouteResponse [cook] viewRoot HTML
  where cook = clearCookie "session"

userJSONField :: BS.ByteString -> Maybe BS.ByteString -> Handler (Maybe JSON.Encoding)
userJSONField "notifications" _ = Just . JSON.toEncoding <$> countUserNotifications
userJSONField _ _ = return Nothing

userHandler :: API -> [(BS.ByteString, BS.ByteString)] -> Action
userHandler api _ =
    withAuth $ do
        method <- peeks Wai.requestMethod
        if method == methodGet && api == JSON then viewUserAction
        else if method == methodPost then postUserAction api
        else error "unhandled api/method combo" -- TODO: better error

-- viewUser :: ActionRoute ()
-- viewUser = action GET (pathJSON </< "user") $ \() -> withAuth $ viewUserAction

viewUserAction :: Handler Response
viewUserAction = do
  ident <- peek
  let i = identityJSON ident
  q <- JSON.jsonQuery userJSONField =<< peeks Wai.queryString
  return $ okResponse [] (i `JSON.foldObjectIntoRec` q)

postUser :: ActionRoute API -- TODO: remove when
postUser = action POST (pathAPI </< "user") $ \api -> withAuth $ postUserAction api

data UpdateUserRequest = UpdateUserRequest () (Maybe BS.ByteString) (Maybe BS.ByteString)

postUserAction :: API -> Handler Response
postUserAction api = do
  auth <- peek
  let acct = siteAccount auth
  auth' <- runForm ((api == HTML) `thenUse` htmlUserForm acct) $ do
    csrfForm
    -- TODO: pass old password into UpdateUserRequest
    "auth" .:> (deformGuard "Incorrect password" . (`checkPassword` auth) =<< deform)
    email <- "email" .:> deformNonEmpty emailTextForm
    passwd <- "password" .:> deformNonEmpty (passwordForm acct)
    let _ = UpdateUserRequest () email passwd
    let acct' = acct
          { accountEmail = fromMaybe (accountEmail acct) email
          , accountParty = (accountParty acct){ partyAccount = Just acct' }
          }
    return auth
      { siteAccount = acct'
      , accountPasswd = passwd <|> accountPasswd auth
      }
  changeAccount auth'
  when (on (/=) (accountEmail . siteAccount) auth' auth || on (/=) accountPasswd auth' auth) $
    createNotification (blankNotification acct NoticeAccountChange) -- use old acct (email)
      { notificationParty = Just $ partyRow $ accountParty acct
      , notificationDelivered = DeliveryAsync -- force immediate delivery
      }
  case api of
    JSON -> return $ okResponse [] $ JSON.recordEncoding $ partyJSON $ accountParty $ siteAccount auth'
    HTML -> peeks $ otherRouteResponse [] viewParty (api, TargetProfile)
