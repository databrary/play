{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables #-}
module Controller.Volume
  ( getVolume
  , viewVolume
  , viewVolumeEdit
  , viewVolumeCreateHandler
  , postVolume
  , createVolume
  -- , viewVolumeLinks
  , postVolumeLinks
  , postVolumeAssist
  , queryVolumes
  , thumbVolume
  , volumeDownloadName
  -- , volumeJSONQuery
  , volumeIsPublicRestricted
  ) where

import Control.Applicative ((<|>), optional)
import Control.Arrow ((&&&), (***))
import Control.Monad (mfilter, guard, void, when, forM_)
-- import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT(..), evalStateT, get, put)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashMap.Strict as HM
import Data.Function (on)
import Data.Int (Int16)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (noContent204, unsupportedMediaType415)
import Network.URI (URI)
import qualified Network.Wai as Wai

import Ops
import Has
import qualified JSON
import Model.Asset (Asset)
import Model.Enum
import Model.Id
import Model.Permission hiding (checkPermission)
import Model.Authorize
import Model.Volume hiding (getVolume)
import Model.VolumeAccess
import Model.Party
import Model.Citation
import Model.Citation.CrossRef
import Model.Funding
import Model.Container
import Model.Record
import Model.VolumeMetric
import Model.RecordSlot
import Model.Segment (Segment)
import Model.Slot
import Model.AssetSlot
import Model.Excerpt
import Model.Tag
import Model.Comment
import Model.VolumeState
import Model.Notification.Types
import Store.Filename
import Static.Service
import Service.Mail
import HTTP.Parse
import HTTP.Form.Deform
import HTTP.Path.Parser
import Action.Route
import Action
import Controller.Paths
import Controller.Permission
import Controller.Form
import Controller.Angular
import Controller.Web
import {-# SOURCE #-} Controller.AssetSegment
import Controller.Notification
import View.Form (FormHtml)
import qualified Model.Volume as Model

-- | Convert 'Model.Volume' into HTTP error responses if the lookup fails or is
-- denied.
getVolume
    :: Permission
    -- ^ Requested permission
    -> Id Volume
    -- ^ Volume to look up
    -> Handler Volume
    -- ^ The volume, as requested (or a short-circuited error response)
getVolume p i = do
  resp <- Model.getVolume p i
  case resp of
    LookupFailed -> result =<< peeks notFoundResponse
    LookupDenied -> result =<< peeks forbiddenResponse
    LookupFound v -> pure v

data VolumeCache = VolumeCache
  { volumeCacheAccess :: Maybe [VolumeAccess]
  , volumeCacheTopContainer :: Maybe Container
  , volumeCacheRecords :: Maybe (HML.HashMap (Id Record) Record)
  }

-- type VolumeCacheHandler a = StateT VolumeCache Handler a

instance Monoid VolumeCache where
  mempty = VolumeCache Nothing Nothing Nothing
  mappend (VolumeCache a1 t1 r1) (VolumeCache a2 t2 r2) = VolumeCache (a1 <|> a2) (t1 <|> t2) (r1 <> r2)

runVolumeCache :: StateT VolumeCache Handler a -> Handler a
runVolumeCache f = evalStateT f mempty

cacheVolumeAccess :: Volume -> Permission -> StateT VolumeCache Handler [VolumeAccess]
cacheVolumeAccess vol perm = do
  vc <- get
  takeWhile ((perm <=) . volumeAccessIndividual) <$>
    fromMaybeM (do
      a <- lookupVolumeAccess vol PermissionNONE
      put vc{ volumeCacheAccess = Just a }
      return a)
      (volumeCacheAccess vc)

cacheVolumeRecords :: Volume -> StateT VolumeCache Handler ([Record], HML.HashMap (Id Record) Record)
cacheVolumeRecords vol = do
  vc <- get
  maybe (do
    l <- lookupVolumeRecords vol
    let m = HML.fromList [ (recordId $ recordRow r, r) | r <- l ]
    put vc{ volumeCacheRecords = Just m }
    return (l, m))
    (return . (HML.elems &&& id))
    (volumeCacheRecords vc)

cacheVolumeTopContainer :: Volume -> StateT VolumeCache Handler Container
cacheVolumeTopContainer vol = do
  vc <- get
  fromMaybeM (do
    t <- lookupVolumeTopContainer vol
    put vc{ volumeCacheTopContainer = Just t }
    return t)
    (volumeCacheTopContainer vc)

leftJoin :: (a -> b -> Bool) -> [a] -> [b] -> [(a, [b])]
leftJoin _ [] [] = []
leftJoin _ [] _ = error "leftJoin: leftovers"
leftJoin p (a:al) b = uncurry (:) $ (,) a *** leftJoin p al $ span (p a) b

volumeIsPublicRestricted :: Volume -> Bool
volumeIsPublicRestricted v =
  case volumeRolePolicy v of
    RolePublicViewer PublicRestrictedPolicy -> True
    RoleSharedViewer SharedRestrictedPolicy -> True
    _ -> False

volumeJSONField :: Volume -> BS.ByteString -> Maybe BS.ByteString -> StateT VolumeCache Handler (Maybe JSON.Encoding)
volumeJSONField vol "access" ma =
  Just . JSON.mapObjects volumeAccessPartyJSON
    <$> cacheVolumeAccess vol (fromMaybe PermissionNONE $ readDBEnum . BSC.unpack =<< ma)
{-
volumeJSONField vol "publicaccess" ma = do
  Just . JSON.toEncoding . show . volumePublicAccessSummary
    <$> cacheVolumeAccess vol (fromMaybe PermissionNONE $ readDBEnum . BSC.unpack =<< ma)
-}
volumeJSONField vol "citation" _ =
  Just . JSON.toEncoding <$> lookupVolumeCitation vol
volumeJSONField vol "links" _ =
  Just . JSON.toEncoding <$> lookupVolumeLinks vol
volumeJSONField vol "funding" _ =
  Just . JSON.mapObjects fundingJSON <$> lookupVolumeFunding vol
volumeJSONField vol "containers" mContainersVal = do
  (cl :: [(Container, [(Segment, Id Record)])]) <- if records
  then lookupVolumeContainersRecordIds vol
  else nope <$> lookupVolumeContainers vol
  (cl' :: [((Container, [(Segment, Id Record)]), [(Asset, SlotId)])]) <- if assets
    then leftJoin (\(c, _) (_, SlotId a _) -> containerId (containerRow c) == a) cl <$> lookupVolumeAssetSlotIds vol
    else return $ nope cl
  rm <- if records then snd <$> cacheVolumeRecords vol else return HM.empty
  let publicRestricted = volumeIsPublicRestricted vol
      br = blankRecord undefined vol
      rjs c (s, r)          = JSON.recordObject $ recordSlotJSON publicRestricted $ RecordSlot (HML.lookupDefault br{ recordRow = (recordRow br){ recordId = r } } r rm) (Slot c s)
      ajs c (a, SlotId _ s) = JSON.recordObject $ assetSlotJSON publicRestricted $ AssetSlot a (Just (Slot c s))
  return $ Just $ JSON.mapRecords (\((c, rl), al) ->
      containerJSON publicRestricted c
      `JSON.foldObjectIntoRec`
            (   (if records then JSON.nestObject "records" (\u -> map (u . rjs c) rl) else mempty)
             <> (if assets  then JSON.nestObject "assets"  (\u -> map (u . ajs c) al) else mempty)))
    cl'
  where
  full = mContainersVal == Just "all"
  assets = full || mContainersVal == Just "assets"
  records = full || mContainersVal == Just "records"
  nope = map (, [])
volumeJSONField vol "top" _ = do
  topCntr <- cacheVolumeTopContainer vol
  let publicRestricted = volumeIsPublicRestricted vol
  (return . Just . JSON.recordEncoding . containerJSON publicRestricted) topCntr
volumeJSONField vol "records" _ = do
  (l, _) <- cacheVolumeRecords vol
  let publicRestricted = volumeIsPublicRestricted vol
  return $ Just $ JSON.mapRecords (recordJSON publicRestricted) l
volumeJSONField vol "metrics" _ =
  let metricsCaching = lookupVolumeMetrics vol
  in (Just . JSON.toEncoding) <$> metricsCaching
volumeJSONField vol "excerpts" _ =
  Just . JSON.mapObjects (\e -> excerptJSON e
    <> "asset" JSON..=: (assetSlotJSON False (view e) -- should publicRestricted be set based on volume?
      `JSON.foldObjectIntoRec` ("container" JSON..= (view e :: Id Container))))
    <$> lookupVolumeExcerpts vol
volumeJSONField vol "tags" n = do
  t <- cacheVolumeTopContainer vol
  tc <- lookupSlotTagCoverage (containerSlot t) (maybe 64 fst $ BSC.readInt =<< n)
  return $ Just $ JSON.mapRecords tagCoverageJSON tc
volumeJSONField vol "comments" n = do
  t <- cacheVolumeTopContainer vol
  tc <- lookupSlotComments (containerSlot t) (maybe 64 fst $ BSC.readInt =<< n)
  return $ Just $ JSON.mapRecords commentJSON tc
volumeJSONField vol "state" _ =
  Just . JSON.toEncoding . JSON.object . map (volumeStateKey &&& volumeStateValue) <$> lookupVolumeState ((volumeId . volumeRow) vol) (volumeRolePolicy vol)
volumeJSONField o "filename" _ =
  return $ Just $ JSON.toEncoding $ makeFilename $ volumeDownloadName o
volumeJSONField _ _ _ = return Nothing

volumeJSONQuery :: Volume -> Maybe [VolumeAccess] -> JSON.Query -> Handler (JSON.Record (Id Volume) JSON.Series)
volumeJSONQuery vol mAccesses q =
  let seriesCaching :: StateT VolumeCache Handler JSON.Series
      seriesCaching = JSON.jsonQuery (volumeJSONField vol) q
      expandedVolJSONcaching :: StateT VolumeCache Handler (JSON.Record (Id Volume) JSON.Series)
      expandedVolJSONcaching = (\series -> volumeJSON vol mAccesses `JSON.foldObjectIntoRec` series) <$> seriesCaching
  in
    runVolumeCache $ expandedVolJSONcaching

volumeDownloadName :: Volume -> [T.Text]
volumeDownloadName v =
  T.pack ("databrary" ++ show (volumeId $ volumeRow v))
    : map (T.takeWhile (',' /=) . snd) (volumeOwners v)
    ++ [fromMaybe (volumeName $ volumeRow v) (getVolumeAlias v)]

viewVolume :: ActionRoute (API, Id Volume)
viewVolume = action GET (pathAPI </> pathId) $ \(api, vi) -> withAuth $ do
  when (api == HTML) angular
  v <- getVolume PermissionPUBLIC vi
  accesses <- lookupVolumeAccess v PermissionNONE
  -- (liftIO . print) ("num accesses", length accesses)
  -- case api of
  let idSeriesRecAct :: Handler (JSON.Record (Id Volume) JSON.Series)
      idSeriesRecAct = volumeJSONQuery v (Just accesses) =<< peeks Wai.queryString
  okResponse [] . JSON.recordEncoding <$> idSeriesRecAct
  {-
    HTML -> do
      top <- lookupVolumeTopContainer v
      t <- lookupSlotKeywords $ containerSlot top
      peeks $ okResponse [] . htmlVolumeView v t
  -}

data CreateOrUpdateVolumeCitationRequest =
    CreateOrUpdateVolumeCitationRequest
        T.Text
        (Maybe T.Text)
        (Maybe T.Text)
        T.Text
        (Maybe URI)
        (Maybe Int16)

volumeForm :: Volume -> DeformHandler f Volume
volumeForm v = do
  name <- "name" .:> deform
  alias <- "alias" .:> deformNonEmpty deform
  body <- "body" .:> deformNonEmpty deform
  return v
    { volumeRow = (volumeRow v)
      { volumeName = name
      , volumeAlias = alias
      , volumeBody = body
      }
    }

-- FIXME: Too impure, and needs test: What elements of the input are modified?
volumeCitationForm :: Volume -> DeformHandler f (Volume, Maybe Citation, CreateOrUpdateVolumeCitationRequest)
volumeCitationForm v = do
  csrfForm
  vol <- volumeForm v
  cite <- "citation" .:> Citation
    <$> ("head" .:> deform)
    <*> ("url" .:> deformNonEmpty deform)
    <*> ("year" .:> deformNonEmpty deform)
    <*> pure Nothing
  let createOrUpdateVolumeCitationRequest =
        CreateOrUpdateVolumeCitationRequest
            ((volumeName . volumeRow) vol)
            ((volumeAlias . volumeRow) vol)
            ((volumeBody . volumeRow) vol)
            (citationHead cite)
            (citationURL cite)
            (citationYear cite)
  look <- flatMapM (lift . focusIO . lookupCitation) $
    guard (T.null (volumeName $ volumeRow vol) || T.null (citationHead cite) || isNothing (citationYear cite)) >> citationURL cite
  let fill = maybe cite (cite <>) look
      empty = T.null (citationHead fill) && isNothing (citationURL fill) && isNothing (citationYear fill)
      name
        | Just title <- citationTitle fill
        , T.null (volumeName $ volumeRow vol) = title
        | otherwise = volumeName $ volumeRow vol
  _ <- "name" .:> deformRequired name
  when (not empty) $ void $
    "citation" .:> "head" .:> deformRequired (citationHead fill)
  return (vol{ volumeRow = (volumeRow vol){ volumeName = name } }, empty `unlessUse` fill, createOrUpdateVolumeCitationRequest)

viewVolumeEdit :: ActionRoute (Id Volume)
viewVolumeEdit = action GET (pathHTML >/> pathId </< "edit") $ \_ -> withAuth $ do
  angular
  return (okResponse [] ("" :: String)) -- should never get here

viewVolumeCreateHandler :: Action  -- TODO : GET only
viewVolumeCreateHandler = withAuth $ do
  angular
  return (okResponse [] ("" :: String)) -- should never get here

postVolume :: ActionRoute (Id Volume)
postVolume = action POST (pathJSON >/> pathId) $ \vi -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  cite <- lookupVolumeCitation v
  (v', cite', _) <- runForm (Nothing :: Maybe (RequestContext -> FormHtml a)) $ volumeCitationForm v
  changeVolume v'
  r <- changeVolumeCitation v' cite'
  return $ okResponse [] $
    JSON.recordEncoding $ volumeJSONSimple v' `JSON.foldObjectIntoRec` ("citation" JSON..= if r then cite' else cite)

data CreateVolumeRequest =
    CreateVolumeRequest (Maybe (Id Party)) CreateOrUpdateVolumeCitationRequest

createVolume :: ActionRoute ()
createVolume = action POST (pathJSON >/> "volume") $ \() -> withAuth $ do
  u <- peek
  (bv, cite, owner) <- runForm (Nothing :: Maybe (RequestContext -> FormHtml a)) $ do
    csrfForm
    (bv, cite, req) <- volumeCitationForm blankVolume
    own <- "owner" .:> do
      oi <- deformOptional deform
      let _ = CreateVolumeRequest oi req
      own <- maybe (return $ Just $ selfAuthorize u) (lift . lookupAuthorizeParent u) $ mfilter (partyId (partyRow u) /=) oi
      deformMaybe' "You are not authorized to create volumes for that owner." $
        authorizeParent . authorization <$> mfilter ((PermissionADMIN <=) . accessMember) own
    auth <- lift $ lookupAuthorization own rootParty
    deformGuard "Insufficient site authorization to create volume." $
      PermissionEDIT <= accessSite auth
    return (bv, cite, own)
  v <- addVolume bv
  _ <- changeVolumeCitation v cite
  setDefaultVolumeAccessesForCreated owner v
  when (on (/=) (partyId . partyRow) owner u) $ forM_ (partyAccount owner) $ \t ->
    createNotification (blankNotification t NoticeVolumeCreated)
      { notificationVolume = Just $ volumeRow v
      , notificationParty = Just $ partyRow owner
      }
  return $ okResponse [] $ JSON.recordEncoding $ volumeJSONSimple v

newtype UpdateVolumeLinksRequest =
    UpdateVolumeLinksRequest [(T.Text, Maybe URI)]

postVolumeLinks :: ActionRoute (Id Volume)
postVolumeLinks = action POST (pathJSON >/> pathId </< "link") $ \vi -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  -- links <- lookupVolumeLinks v
  links' <- runForm (Nothing :: Maybe (RequestContext -> FormHtml a)) $ do
    csrfForm
    res <- withSubDeforms $ \_ -> Citation
      <$> ("head" .:> deform)
      <*> ("url" .:> (Just <$> deform))
      <*> pure Nothing
      <*> pure Nothing
    let _ = UpdateVolumeLinksRequest (fmap (\c -> (citationHead c, citationURL c)) res)
    pure res
  changeVolumeLinks v links'
  return $ okResponse [] $ JSON.recordEncoding $ volumeJSONSimple v `JSON.foldObjectIntoRec` ("links" JSON..= links')
  -- HTML -> peeks $ otherRouteResponse [] viewVolume arg

postVolumeAssist :: ActionRoute (Id Volume)
postVolumeAssist = action POST (pathJSON >/> pathId </< "assist") $ \vi -> withAuth $ do
  user <- authAccount
  v <- getVolume PermissionEDIT vi
  addr <- peeks staticAssistAddr
  cont <- parseRequestContent (const 0)
  body <- case cont :: Content () of
    ContentText body -> return body
    _ -> result $ emptyResponse unsupportedMediaType415 []
  sendMail [Left addr] [Right user] ("Databrary upload assistance request for volume " <> T.pack (show vi)) $ TL.fromChunks
    [ partyName $ partyRow $ accountParty user, " has requested curation assistance for ", volumeName $ volumeRow v, "\n\n" ] <> body `TL.snoc` '\n'
  createVolumeNotification v ($ NoticeVolumeAssist)
  return $ emptyResponse noContent204 []

volumeSearchForm :: DeformHandler f VolumeFilter
volumeSearchForm = VolumeFilter
  <$> ("query" .:> deformNonEmpty deform)
  <*> ("party" .:> optional deform)
  <*> paginateForm

queryVolumes :: ActionRoute API
queryVolumes = action GET (pathAPI </< "volume") $ \api -> withAuth $ do
  when (api == HTML) angular
  vf <- runForm (Nothing :: Maybe (RequestContext -> FormHtml a)) volumeSearchForm
  p <- findVolumes vf
  return $ okResponse [] $ JSON.mapRecords volumeJSONSimple p
  -- HTML -> peeks $ blankForm . htmlVolumeSearch vf p

thumbVolume :: ActionRoute (Id Volume)
thumbVolume = action GET (pathId </< "thumb") $ \vi -> withAuth $ do
  v <- getVolume PermissionPUBLIC vi
  e <- lookupVolumeThumb v
  maybe
    (peeks $ otherRouteResponse [] webFile (Just $ staticPath ["images", "draft.png"]))
    (\as -> peeks $ otherRouteResponse [] downloadAssetSegment (slotId $ view as, view as))
    e
