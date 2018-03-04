{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Register
  ( passwordResetHandler
  , viewPasswordReset
  , postPasswordReset
  , registerHandler
  , viewRegister
  , postRegister
  , resendInvestigatorHandler
  , resendInvestigator
  ) where

import Control.Applicative ((<$>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Network.Wai as Wai
import Network.HTTP.Types.Method (methodGet, methodPost)
import qualified Network.HTTP.Types.Method as HTM
import Servant (FromHttpApiData(..))

import Databrary.Ops
import Databrary.Has
import Databrary.Service.Mail
import Databrary.Static.Fillin
import Databrary.Model.Permission
import Databrary.Model.Id
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Model.Token
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.Permission
import Databrary.Controller.Party
import Databrary.Controller.Token
import Databrary.Controller.Angular
import Databrary.View.Register

resetPasswordMail :: Either BS.ByteString SiteAuth -> T.Text -> (Maybe TL.Text -> TL.Text) -> ActionM ()
resetPasswordMail (Left email) subj body =
  sendMail [Left email] [] subj (body Nothing)
resetPasswordMail (Right auth) subj body = do
  tok <- loginTokenId =<< createLoginToken auth True
  req <- peek
  sendMail [Right $ view auth] [] subj
    (body $ Just $ TLE.decodeLatin1 $ BSB.toLazyByteString $ actionURL (Just req) viewLoginToken (HTML, tok) [])

registerHandler :: API -> HTM.Method -> [(BS.ByteString, BS.ByteString)] -> Action
registerHandler api method _
    | method == methodGet && api == HTML = viewRegisterAction
    | method == methodPost = postRegisterAction api
    | otherwise = error "unhandled api/method combo" -- TODO: better error 

viewRegister :: ActionRoute ()
viewRegister = action GET (pathHTML </< "user" </< "register") $ \() -> viewRegisterAction

viewRegisterAction :: Action
viewRegisterAction = withAuth $ do
  angular
  maybeIdentity
    (peeks $ blankForm . htmlRegister)
    (\_ -> peeks $ otherRouteResponse [] viewParty (HTML, TargetProfile))

postRegister :: ActionRoute API
postRegister = action POST (pathAPI </< "user" </< "register") $ postRegisterAction

postRegisterAction :: API -> Action
postRegisterAction = \api -> withoutAuth $ do
  reg <- runForm (api == HTML ?> htmlRegister) $ do
    name <- "sortname" .:> (deformRequired =<< deform)
    prename <- "prename" .:> deformNonEmpty deform
    email <- "email" .:> emailTextForm
    affiliation <- "affiliation" .:> deformNonEmpty deform
    _ <- "agreement" .:> (deformCheck "You must consent to the user agreement." id =<< deform)
    let p = blankParty
          { partyRow = (partyRow blankParty)
            { partySortName = name
            , partyPreName = prename
            , partyAffiliation = affiliation
            }
          , partyAccount = Just a
          }
        a = Account
          { accountParty = p
          , accountEmail = email
          }
    return a
  auth <- maybe (SiteAuth <$> addAccount reg <*> pure Nothing <*> pure mempty) return =<< lookupSiteAuthByEmail False (accountEmail reg)
  resetPasswordMail (Right auth)
    "Databrary account created"
    $ \(Just url) ->
      "Thank you for registering with Databrary. Please use this link to complete your registration:\n\n"
      <> url <> "\n\n\
      \By clicking the above link, you also indicate that you have read and understand the Databrary Access agreement, which you can download here: http://databrary.org/policies/agreement.pdf\n\n\
      \Once you've validated your e-mail, you will be able to request authorization to be granted full access to Databrary.\n"
  focusIO $ staticSendInvestigator (view auth)
  return $ okResponse [] $ "Your confirmation email has been sent to '" <> accountEmail reg <> "'."

resendInvestigator :: ActionRoute (Id Party)
resendInvestigator = action POST (pathHTML >/> pathId </< "investigator") $
    \i -> resendInvestigatorHandler [("partyId", (BSC.pack . show) i)]

resendInvestigatorHandler :: [(BS.ByteString, BS.ByteString)] -> Action
resendInvestigatorHandler params = withAuth $ do  -- TODO: handle POST only
  let paramId = maybe (error "partyId missing") TE.decodeUtf8 (lookup "partyId" params)
  let i = either (error . show) Id (parseUrlPiece paramId)
  checkMemberADMIN
  p <- getParty (Just PermissionREAD) (TargetParty i)
  focusIO $ staticSendInvestigator p
  return $ okResponse [] ("sent" :: String)

passwordResetHandler :: API -> HTM.Method -> [(BS.ByteString, BS.ByteString)] -> Action
passwordResetHandler api method _
    | method == methodGet && api == HTML = viewPasswordResetAction
    | method == methodPost = postPasswordResetAction api
    | otherwise = error "unhandled api/method combo" -- TODO: better error 

viewPasswordReset :: ActionRoute ()
viewPasswordReset = action GET (pathHTML </< "user" </< "password") $ \() -> viewPasswordResetAction

viewPasswordResetAction :: Action
viewPasswordResetAction = withoutAuth $ do
  angular
  peeks $ blankForm . htmlPasswordReset

postPasswordReset :: ActionRoute API
postPasswordReset = action POST (pathAPI </< "user" </< "password") $ postPasswordResetAction

postPasswordResetAction :: API -> Action
postPasswordResetAction = \api -> withoutAuth $ do
  email <- runForm (api == HTML ?> htmlPasswordReset) $ do
    "email" .:> emailTextForm
  auth <- lookupPasswordResetAccount email
  resetPasswordMail (maybe (Left email) Right auth)
    "Databrary password reset" $
    ("Someone (hopefully you) has requested to reset the password for the Databrary account associated with this email address. If you did not request this, let us know (by replying to this message) or simply ignore it.\n\n" <>)
    . maybe
      "Unfortunately, no Databrary account was found for this email address. You can try again with a different email address, or reply to this email for assistance.\n"
      ("Otherwise, you may use this link to reset your Databrary password:\n\n" <>)
  return $ okResponse [] $ "Your password reset information has been sent to '" <> email <> "'."

