{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Authorize
  ( viewAuthorize
  , postAuthorize
  , deleteAuthorize
  , postAuthorizeNotFound
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when, liftM3, mfilter, forM_)
import Data.Function (on)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime(..), fromGregorian, addGregorianYearsRollOver)
import Network.HTTP.Types (noContent204)

import Databrary.Ops
import Databrary.Has (peek, peeks)
import qualified Databrary.JSON as JSON
import Databrary.Service.Mail
import Databrary.Static.Service
import Databrary.Model.Id.Types
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Identity
import Databrary.Model.Notification.Types
import Databrary.Model.Authorize
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Form.Deform
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.Party
import Databrary.Controller.Notification
import Databrary.View.Authorize

viewAuthorize :: ActionRoute (API, PartyTarget, AuthorizeTarget)
viewAuthorize = action GET (pathAPI </>> pathPartyTarget </> pathAuthorizeTarget) $ \(api, i, AuthorizeTarget app oi) -> withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  o <- maybeAction =<< lookupParty oi
  let (child, parent) = if app then (p, o) else (o, p)
  c <- lookupAuthorize child parent
  let c' = Authorize (Authorization mempty child parent) Nothing `fromMaybe` c
  case api of
    JSON -> return $ okResponse [] $ JSON.objectEncoding $ authorizeJSON c'
    HTML
      | app -> return $ okResponse [] ("" :: T.Text) -- TODO
      | otherwise -> peeks $ blankForm . htmlAuthorizeForm c'

partyDelegates :: Party -> ActionM [Account]
partyDelegates u = do
  l <- deleg u
  if null l
    then deleg rootParty
    else return l
  where
  deleg p = mapMaybe partyAccount . (p :)
    . map (authorizeChild . authorization)
    <$> lookupAuthorizedChildren p (Just PermissionADMIN)

removeAuthorizeNotify :: Maybe Authorize -> Maybe Authorize -> ActionM ()
removeAuthorizeNotify priorAuth =
    let noReplacementAuthorization = Nothing
    in updateAuthorize priorAuth noReplacementAuthorization

-- | Remove (only first argument provided) or swap in new authorization, triggering notifications
updateAuthorize :: Maybe Authorize -> Maybe Authorize -> ActionM ()
updateAuthorize priorAuth newAuth
  | Just auth <- authorization <$> (priorAuth <|> newAuth :: Maybe Authorize) = do
  maybe
    (mapM_ removeAuthorize priorAuth)
    changeAuthorize
    newAuth
  when (on (/=) (foldMap $ authorizeAccess . authorization) newAuth priorAuth) $ do
    let perm = accessSite <$> newAuth
    dl <- partyDelegates $ authorizeParent auth
    forM_ dl $ \t ->
      createNotification (blankNotification t NoticeAuthorizeChildGranted)
        { notificationParty = Just $ partyRow $ authorizeChild auth
        , notificationPermission = perm
        }
    forM_ (partyAccount $ authorizeChild auth) $ \t ->
      createNotification (blankNotification t NoticeAuthorizeGranted)
        { notificationParty = Just $ partyRow $ authorizeParent auth
        , notificationPermission = perm
        }
  updateAuthorizeNotifications priorAuth
      $ fromMaybe (Authorize auth{ authorizeAccess = mempty } Nothing) newAuth
updateAuthorize ~Nothing ~Nothing = return ()

postAuthorize :: ActionRoute (API, PartyTarget, AuthorizeTarget)
postAuthorize = action POST (pathAPI </>> pathPartyTarget </> pathAuthorizeTarget) $ \arg@(api, i, AuthorizeTarget app oi) -> withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  o <- maybeAction . mfilter ((0 <) . unId . partyId . partyRow) =<< lookupParty oi
  let (child, parent) = if app then (p, o) else (o, p)
  c <- lookupAuthorize child parent
  let c' = Authorize (Authorization mempty child parent) Nothing `fromMaybe` c
  a <- if app
    then do
      when (isNothing c) $ do
        changeAuthorize c'
        dl <- partyDelegates o
        forM_ dl $ \t ->
          createNotification (blankNotification t NoticeAuthorizeChildRequest)
            { notificationParty = Just $ partyRow o }
        forM_ (partyAccount p) $ \t ->
          createNotification (blankNotification t NoticeAuthorizeRequest)
            { notificationParty = Just $ partyRow o }
      return $ Just c'
    else do
      su <- peeks identityAdmin
      now <- peek
      let maxexp = addGregorianYearsRollOver 2 $ utctDay now
          minexp = fromGregorian 2000 1 1
      a <- runForm (api == HTML ?> htmlAuthorizeForm c') $ do
        csrfForm
        delete <- "delete" .:> deform
        delete ?!$> do
          site <- "site" .:> deform
          member <- "member" .:> deform
          expires <- "expires" .:> (deformCheck "Expiration must be within two years." (all (\e -> su || e > minexp && e <= maxexp))
            =<< (<|> (su ?!> maxexp)) <$> deformNonEmpty deform)
          return $ Authorize (Authorization (Access site member) child parent) $ fmap (`UTCTime` 43210) expires
      updateAuthorize c a
      return a
  case api of
    JSON -> return $ okResponse [] $ JSON.objectEncoding $ foldMap authorizeJSON a <> "party" JSON..=: partyJSON o
    HTML -> peeks $ otherRouteResponse [] viewAuthorize arg

deleteAuthorize :: ActionRoute (API, PartyTarget, AuthorizeTarget)
deleteAuthorize = action DELETE (pathAPI </>> pathPartyTarget </> pathAuthorizeTarget) $ \arg@(api, i, AuthorizeTarget apply oi) -> withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  (o :: Party) <- do
      mAuthorizeTargetParty <- lookupParty oi
      maybeAction mAuthorizeTargetParty
  let (child, parent) = if apply then (p, o) else (o, p) -- Delete request you sent vs request you recvd?
  mAuth <- lookupAuthorize child parent
  removeAuthorizeNotify mAuth
  case api of
    JSON -> return $ okResponse [] $ JSON.objectEncoding $ "party" JSON..=: partyJSON o
    HTML -> peeks $ otherRouteResponse [] viewAuthorize arg

postAuthorizeNotFound :: ActionRoute (PartyTarget)
postAuthorizeNotFound = action POST (pathJSON >/> pathPartyTarget </< "notfound") $ \i -> withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  agent <- peeks $ fmap accountEmail . partyAccount
  (name, perm, info) <- runForm Nothing $ liftM3 (,,)
    ("name" .:> deform)
    ("permission" .:> deform)
    ("info" .:> deformNonEmpty deform)
  authaddr <- peeks staticAuthorizeAddr
  title <- peeks $ authorizeSiteTitle perm
  sendMail [Left authaddr] []
    ("Databrary authorization request from " <> partyName (partyRow p))
    $ TL.fromChunks [partyName $ partyRow p, " <", foldMap TE.decodeLatin1 agent, ">", mbt $ partyAffiliation $ partyRow p, " has requested to be authorized as an ", title, " by ", name, mbt info, ".\n"]
  return $ emptyResponse noContent204 []
  where mbt = maybe "" $ \t -> " (" <> t <> ")"
