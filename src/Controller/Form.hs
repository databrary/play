{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Controller.Form
  ( FormData
  , DeformHandler
  , runFormFiles
  , runForm
  , blankForm

  , emailTextForm
  , passwordForm
  , paginateForm
  , csrfForm
  ) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import qualified Crypto.BCrypt as BCrypt
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower)
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import Network.HTTP.Types (badRequest400)
import qualified Text.Blaze.Html5 as Html
import qualified Text.Regex.Posix as Regex

import Has
import Model.Paginate
import Model.Party
import Model.Identity
import Service.Passwd
import HTTP.Parse (FileContent)
import HTTP.Form (FormData)
import HTTP.Form.Deform
import HTTP.Form.View (runFormView, blankFormView)
import HTTP.Form.Errors (FormErrors)
import Action.Response
import Action.Types
import Action.Form (getFormData)
import Controller.Permission (checkVerfHeader)
import View.Form (FormHtml)

-- FIXME: This is too impure: each value of this type should be decomposed separately
-- into a DeformT and an Handler e.g. deformT ... >>= \x -> actionM ...
type DeformHandler f a = DeformT f Handler a

jsonFormErrors :: FormErrors -> Response
jsonFormErrors = response badRequest400 [] . JSON.toEncoding

htmlFormErrors :: (FormErrors -> Html.Html) -> FormErrors -> Response
htmlFormErrors f = response badRequest400 [] . f

handleForm :: (FormErrors -> Response) -> Either FormErrors a -> Handler a
handleForm re = either (result . re) return

handleFormErrors :: Maybe (FormErrors -> Html.Html) -> Either FormErrors a -> Handler a
handleFormErrors = handleForm . maybe jsonFormErrors htmlFormErrors

runFormWith :: FormData f -> Maybe (RequestContext -> FormHtml f) -> DeformHandler f a -> Handler a
runFormWith fd mf fa = do
  req <- ask
  let fv hv = runFormView (hv req) fd
  handleFormErrors (fv <$> mf) =<< runDeform fa fd

runFormFiles
  :: FileContent f => [(BS.ByteString, Word64)] -> Maybe (RequestContext -> FormHtml f) -> DeformHandler f a -> Handler a
runFormFiles fl mf fa = do
  (fd :: FormData a) <- getFormData fl
  runFormWith fd mf fa

runForm :: Maybe (RequestContext -> FormHtml ()) -> DeformHandler () a -> Handler a
runForm = runFormFiles []

blankForm :: FormHtml f -> Response
blankForm = okResponse [] . blankFormView

emailRegex :: Regex.Regex
emailRegex = Regex.makeRegexOpts Regex.compIgnoreCase Regex.blankExecOpt
  ("^[-a-z0-9!#$%&'*+/=?^_`{|}~.]*@[a-z0-9][a-z0-9\\.-]*[a-z0-9]\\.[a-z][a-z\\.]*[a-z]$" :: String)

emailTextForm :: DeformHandler f BS.ByteString
emailTextForm = do
  e <- deformCheck "Invalid email address" (Regex.matchTest emailRegex) =<< deform
  return $ maybe e (uncurry ((. BSC.map toLower) . (<>)) . flip BS.splitAt e) $ BSC.elemIndex '@' e

passwordForm :: Account -> DeformHandler f BS.ByteString
passwordForm acct = do
  p <- "once" .:> do
    p <- deform
    deformGuard "Password too short. Must be 7 characters." (7 <= BS.length p)
    c <- lift $ focusIO $ passwdCheck p (accountEmail acct) (TE.encodeUtf8 $ partyName $ partyRow $ accountParty acct)
    mapM_ (deformError . ("Insecure password: " <>) . TE.decodeLatin1) c
    return p
  "again" .:> do
    a <- deform
    deformGuard "Passwords do not match." (a == p)
  pw <- liftIO $ BCrypt.hashPasswordUsingPolicy passwordPolicy p
  deformMaybe' "Error processing password." pw

paginateForm :: DeformHandler f Paginate
paginateForm = Paginate
  <$> get "offset" paginateOffset
  <*> get "limit" paginateLimit
  where get t f = t .:> (deformCheck ("invalid " <> t) (\i -> i >= f minBound && i <= f maxBound) =<< deform) <|> return (f def)

csrfForm :: DeformHandler f ()
csrfForm = do
  r <- lift checkVerfHeader
  unless r $ do
    verf <- lift $ peeks identityVerf
    "csverf" .:> maybe
      (deformError "You must be logged in to perform this request.")
      (\v -> deformGuard "Invalid form verifier. Please logout, reload, and try again." . (v ==) =<< deform)
      verf
