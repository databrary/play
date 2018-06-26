{-# LANGUAGE OverloadedStrings #-}
module Controller.Party
  ( getParty
  , viewParty
  , viewPartyEdit
  -- , viewPartyCreate
  , viewPartyCreateHandler
  , viewPartyDelete
  , postParty
  , createParty
  , deleteParty
  , viewAvatar
  , queryParties
  , adminParties
  , adminPartiesHandler
  , csvPartiesHandler
  , csvDuplicatePartiesHandler
  ) where

import Control.Applicative (optional)
import Control.Monad (unless, when, forM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (badRequest400)
import Network.URI (URI)
import qualified Network.Wai as Wai
import Network.Wai.Parse (FileInfo(..))

import Ops
import Has
import qualified JSON as JSON
import Model.Enum
import Model.Id
import Model.Permission
import Model.Release
import Model.Party
import Model.ORCID
import Model.Authorize
import Model.Volume
import Model.VolumeAccess
import Model.Asset
import Model.AssetSlot
import Model.AssetSegment
import Model.Format
import Model.Notification (Notice(NoticeNewsletter), lookupNoticePartyAuthorization)
import Store.Temp
import HTTP.Path.Parser
import HTTP.Form.Deform
import Action.Route
import Action.Types
import Action.Run
import Action
import Controller.Paths
import Controller.Permission
import Controller.Form
import Controller.Angular
import Controller.AssetSegment
import Controller.Web
import Controller.CSV
import View.Party

-- | Ensure the current user has the requested permission for the given party.
getParty
  :: Maybe Permission
  -> PartyTarget
  -> Handler Party
getParty (Just p) (TargetParty i) =
  (\party -> checkPermission2 partyPermission p party) =<< maybeAction =<< lookupAuthParty i
getParty _ mi = do
  u <- accountParty <$> authAccount
  let isme TargetProfile = True
      isme (TargetParty i) = partyId (partyRow u) == i
  unless (isme mi) $ result =<< peeks forbiddenResponse
  return u

partyJSONField :: Party -> BS.ByteString -> Maybe BS.ByteString -> Handler (Maybe JSON.Encoding)
partyJSONField p "parents" o = do
  now <- peek
  fmap (Just . JSON.mapObjects id) . mapM (\a -> do
    let ap = authorizeParent (authorization a)
    acc <- if auth && authorizeActive a now then Just . accessSite <$> lookupAuthorization ap rootParty else return Nothing
    return $ (if admin then authorizeJSON a else mempty)
      <> "party" JSON..=: (partyJSON ap `JSON.foldObjectIntoRec` ("authorization" `JSON.kvObjectOrEmpty` acc))
      <> "expired" `JSON.kvObjectOrEmpty` (True `useWhen` (admin && authorizeExpired a now)))
    -- if the current user doesn't have admin over the party in question, then filter out
    -- inactive authorizations
    =<< lookupAuthorizedParents p (admin `unlessUse` PermissionNONE)
  where
  admin = partyPermission p >= PermissionADMIN
  auth = admin && o == Just "authorization"
partyJSONField p "children" _ =
  Just . JSON.mapObjects (\a ->
    let ap = authorizeChild (authorization a) in
    (if admin then authorizeJSON a else mempty) <> "party" JSON..=: partyJSON ap)
    -- if the current user doesn't have admin over the party in question, then filter out
    -- inactive authorizations
    <$> lookupAuthorizedChildren p (admin `unlessUse` PermissionNONE)
  where admin = partyPermission p >= PermissionADMIN
partyJSONField p "volumes" o = thenReturn (partyPermission p >= PermissionADMIN) $ do
    vols <- lookupPartyVolumes p PermissionREAD
    (fmap (JSON.mapRecords id) . mapM vf) vols
  where
  vf v
    | o == Just "access" = do
      a <- lookupVolumeAccess v (succ PermissionNONE)
      accesses <- lookupVolumeAccess v PermissionNONE  -- TODO: why different perm level
      return $
        volumeJSON v (Just accesses) `JSON.foldObjectIntoRec`
          (JSON.nestObject "access" (\u -> map (u . volumeAccessPartyJSON) a))
    | otherwise = return $ volumeJSONSimple v
partyJSONField p "access" ma = do
  Just . JSON.mapObjects volumeAccessVolumeJSON
    <$> lookupPartyVolumeAccess p (fromMaybe PermissionEDIT $ readDBEnum . BSC.unpack =<< ma)
partyJSONField p "authorization" _ = do
  Just . JSON.toEncoding . accessSite <$> lookupAuthorization p rootParty
partyJSONField _ _ _ = return Nothing

partyJSONQuery :: Party -> JSON.Query -> Handler (JSON.Record (Id Party) JSON.Series)
partyJSONQuery p q = (partyJSON p `JSON.foldObjectIntoRec`) <$> JSON.jsonQuery (partyJSONField p) q

viewParty :: ActionRoute (API, PartyTarget)
viewParty = action GET (pathAPI </> pathPartyTarget) $ \(api, i) -> withAuth $ do
  when (api == HTML) angular
  p <- getParty (Just PermissionNONE) i
  case api of
    JSON -> okResponse [] <$> (partyJSONQuery p =<< peeks Wai.queryString)
    HTML -> peeks $ okResponse [] . htmlPartyView p

data ProcessPartyRequest =
    ProcessPartyRequest Text (Maybe Text) (Maybe ORCID) (Maybe Text) (Maybe URI) (Maybe (Maybe (FileInfo TempFile, Format)))

-- | Extract values to build up the fields of a Party from an incoming form/json request.
-- One part of the extraction is reading and saving any avater image provided.
-- If an image is provided, also generate an asset, that is attached to the core volume.
-- Primarily extracting values that correspond to a PartyRow, all other values (account, permissions) are
-- taken from blankParty or the existing party.
-- This is used by createParty with no party provided, and postParty with existing party provided.
processParty
  :: API -- ^ Whether this request is being handled as part of a server side form handler, or a client side API request
  -> Maybe Party -- ^ The existing version of the party, before any updates
  -> Handler (Party, Maybe (Maybe Asset)) -- ^ A party object populated with the request input and a possible avatar asset.
processParty api p = do
  (p', a) <- runFormFiles [("avatar", maxAvatarSize)] ((api == HTML) `thenUse` (htmlPartyEdit p)) $ do
    csrfForm
    name <- "sortname" .:> (deformRequired =<< deform)
    prename <- "prename" .:> deformNonEmpty deform
    mOrcid <- "orcid" .:> deformNonEmpty (deformRead blankORCID)
    affiliation <- "affiliation" .:> deformNonEmpty deform
    url <- "url" .:> deformNonEmpty deform
    (avatar :: (Maybe (Maybe (FileInfo TempFile, Format)))) <- "avatar" .:> do
      mFileInfo <- deform
      (maybe
         (deformOptional $ return Nothing)
         (\avatarFileInfo -> do
            format <- do
              fmt <-
                deformMaybe' "Unknown or unsupported file format." (getFormatByFilename (fileName avatarFileInfo))
              deformCheck "Must be an image." formatIsImage fmt
            return $ Just $ Just (avatarFileInfo, format))
         mFileInfo)
    let _ = ProcessPartyRequest name prename mOrcid affiliation url avatar
    return (bp
      { partyRow = (partyRow bp)
        { partySortName = name
        , partyPreName = prename
        , partyORCID = mOrcid
        , partyAffiliation = affiliation
        , partyURL = url
        }
      }, avatar)
  a' <- forM a $ mapM $ \(af, fmt) -> do
    let ba = blankAsset coreVolume
    a' <- addAsset ba
      { assetRow = (assetRow ba)
        { assetFormat = fmt
        , assetRelease = Just ReleasePUBLIC
        , assetName = Just $ TE.decodeUtf8 $ fileName af
        }
      } $ Just $ tempFilePath (fileContent af)
    focusIO $ releaseTempFile $ fileContent af
    return a'
  return (p', a')
  where
  maxAvatarSize = 10*1024*1024
  bp = fromMaybe blankParty p

viewPartyEdit :: ActionRoute PartyTarget
viewPartyEdit = action GET (pathHTML >/> pathPartyTarget </< "edit") $ \i -> withAuth $ do
  angular
  p <- getParty (Just PermissionEDIT) i
  peeks $ blankForm . htmlPartyEdit (Just p)

viewPartyCreateHandler :: Action
viewPartyCreateHandler = withAuth $ do
  checkMemberADMIN
  peeks $ blankForm . htmlPartyEdit Nothing

postParty :: ActionRoute (API, PartyTarget)
postParty = multipartAction $ action POST (pathAPI </> pathPartyTarget) $ \(api, i) -> withAuth $ do
  p <- getParty (Just PermissionEDIT) i
  (p', a) <- processParty api (Just p)
  changeParty p'
  mapM_ (changeAvatar p') a
  case api of
    JSON -> return $ okResponse [] $ JSON.recordEncoding $ partyJSON p'
    HTML -> peeks $ otherRouteResponse [] viewParty (api, i)

-- | Create a new party, starting from blankParty, then overlaying data extracted from request.
-- Since this overlaps with how registration creates an account/party combo, this is only used when creating
-- parties manually by a superadmin, such as creating institution parties which don't have an account.
createParty :: ActionRoute API
createParty = multipartAction $ action POST (pathAPI </< "party") $ \api -> withAuth $ do
  checkMemberADMIN
  (bp, a) <- processParty api Nothing
  p <- addParty bp
  mapM_ (changeAvatar p) a
  case api of
    JSON -> return $ okResponse [] $ JSON.recordEncoding $ partyJSON p
    HTML -> peeks $ otherRouteResponse [] viewParty (api, TargetParty $ partyId $ partyRow p)

deleteParty :: ActionRoute (Id Party)
deleteParty = action POST (pathHTML >/> pathId </< "delete") $ \i -> withAuth $ do
  checkMemberADMIN
  p <- getParty (Just PermissionADMIN) (TargetParty i)
  r <- removeParty p
  return $ if r
    then okResponse [] $ partyName (partyRow p) <> " deleted"
    else response badRequest400 [] $ partyName (partyRow p) <> " not deleted"

viewPartyDelete :: ActionRoute (Id Party)
viewPartyDelete = action GET (pathHTML >/> pathId </< "delete") $ \i -> withAuth $ do
  checkMemberADMIN
  p <- getParty (Just PermissionADMIN) (TargetParty i)
  peeks $ blankForm . htmlPartyDelete p

viewAvatar :: ActionRoute (Id Party)
viewAvatar = action GET (pathId </< "avatar") $ \i -> withoutAuth $
  maybe
    (peeks $ otherRouteResponse [] webFile (Just $ staticPath ["images", "avatar.png"]))
    (serveAssetSegment False . assetSlotSegment . assetNoSlot)
    =<< lookupAvatar i

partySearchForm :: DeformHandler f PartyFilter
partySearchForm = PartyFilter
  <$> ("query" .:> deformNonEmpty deform)
  <*> ("authorization" .:> optional deform)
  <*> ("institution" .:> deformNonEmpty deform)
  <*> paginateForm

queryParties :: ActionRoute API
queryParties = action GET (pathAPI </< "party") $ \api -> withAuth $ do
  when (api == HTML) angular
  pf <- runForm ((api == HTML) `thenUse` (htmlPartySearch mempty [])) partySearchForm
  p <- findParties pf
  case api of
    JSON -> return $ okResponse [] $ JSON.mapRecords partyJSON p
    HTML -> peeks $ blankForm . htmlPartySearch pf p

adminParties :: ActionRoute ()
adminParties = action GET ("party" </< "admin") $ \() -> adminPartiesHandler

adminPartiesHandler :: Action  --TODO: GET only
adminPartiesHandler = withAuth $ do
  checkMemberADMIN
  pf <- runForm (Just $ htmlPartyAdmin mempty []) partySearchForm
  p <- findParties pf
  peeks $ blankForm . htmlPartyAdmin pf p

csvPartiesHandler :: Action -- TODO: GET only
csvPartiesHandler = withAuth $ do
  checkMemberADMIN
  pl <- lookupNoticePartyAuthorization NoticeNewsletter
  return $ csvResponse
    [ [ BSC.pack $ show $ partyId $ partyRow p
      , TE.encodeUtf8 $ partySortName $ partyRow p
      , c TE.encodeUtf8 $ partyPreName $ partyRow p
      , c accountEmail $ partyAccount p
      , c (BSC.pack . show) a
      , BSC.pack $ show $ fromEnum d
      ]
    | (p, a, d) <- pl ] "party"
  where c = maybe BS.empty

csvDuplicatePartiesHandler :: Action -- TODO: GET only
csvDuplicatePartiesHandler = withAuth $ do
  checkMemberADMIN
  ps <- getDuplicateParties
  return $ csvResponse
    [ [ BSC.pack $ show $ partyId $ partyRow1
      , TE.encodeUtf8 $ partySortName $ partyRow1
      , maybeEmpty TE.encodeUtf8 (partyPreName $ partyRow1)
      ]
    | partyRow1 <- ps ] "party"
  where
    maybeEmpty :: (a -> BS.ByteString) -> Maybe a -> BS.ByteString
    maybeEmpty = maybe BS.empty
