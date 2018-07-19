{-# LANGUAGE OverloadedStrings #-}
module Controller.Asset
  ( getAsset
  -- , getOrigAsset
  , assetJSONField
  , viewAsset
  , AssetTarget(..)
  , postAsset
  -- , viewAssetEdit
  , createAsset
  -- , viewAssetCreate
  , createSlotAsset
  -- , viewSlotAssetCreate
  , deleteAsset
  , downloadAsset
  , downloadOrigAsset
  , thumbAsset
  , assetDownloadName
  ) where

import Control.Applicative ((<|>))
import Control.Monad ((<=<), void, guard, when)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, isNothing, isJust, maybeToList)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.PostgreSQL.Typed.Range as Range
import Network.HTTP.Types (conflict409)
import qualified Network.Wai as Wai
import Network.Wai.Parse (FileInfo(..))
import qualified System.FilePath.Posix as FPP (makeValid)
import qualified System.FilePath.Windows as FPW (makeValid)

import Ops
import Has
import qualified JSON
import Model.Segment
import Model.Permission hiding (checkPermission)
import Model.Release
import Model.Id
import Model.Volume hiding (getVolume)
import Model.Container
import Model.Token
import Model.Format
import Model.Asset
import Model.Slot
import Model.AssetSlot
import Model.AssetSegment
import Model.Excerpt
import Model.AssetRevision
import Model.Transcode
import Model.Notification
import Files hiding ((</>))
import Store.AV (AV)
import Store.Types
import Store.Asset
import Store.Upload
import Store.Temp
import Store.Transcode
import Store.AV (avProbeLength)
import Store.Probe
import HTTP.Request
import HTTP.Form.Errors
import HTTP.Form.Deform
import HTTP.Path.Parser
import Action
import Controller.Paths
import Controller.Permission
import Controller.Form
import Controller.Volume
import Controller.Slot
import Controller.Format
import Controller.Notification
import {-# SOURCE #-} Controller.AssetSegment
-- import View.Asset
import View.Form (FormHtml)

import Control.Monad.IO.Class

getAsset :: Bool -> Permission -> Bool -> Id Asset -> Handler AssetSlot
getAsset getOrig p checkDataPerm i = do
  mAssetSlot <- (if getOrig then lookupOrigAssetSlot else lookupAssetSlot) i
  slot <- maybeAction mAssetSlot
  void (checkPermission (extractPermissionIgnorePolicy . getAssetSlotVolumePermission2) p slot)
  when checkDataPerm $ do
    -- TODO: delete
    -- liftIO $ print ("checking data perm", "assetSlot", slot)
    -- liftIO $ print ("checking data perm", "seg rlses", getAssetSlotRelease2 slot,
    --                 "vol prm", getAssetSlotVolumePermission2 slot)
    -- liftIO $ print ("result perm", dataPermission4 getAssetSlotRelease2 getAssetSlotVolumePermission2 slot)
    void (userCanReadData getAssetSlotRelease2 getAssetSlotVolumePermission2 slot)
  pure slot

assetJSONField :: AssetSlot -> BS.ByteString -> Maybe BS.ByteString -> Handler (Maybe JSON.Encoding)
assetJSONField a "container" _ =
  return $ JSON.recordEncoding . containerJSON False . slotContainer <$> assetSlot a -- containerJSON should consult volume
assetJSONField a "creation" _ | (extractPermissionIgnorePolicy . getAssetSlotVolumePermission2) a >= PermissionEDIT = do
  (t, n) <- assetCreation $ slotAsset a
  return $ Just $ JSON.pairs $
       "date" `JSON.kvObjectOrEmpty` t
    <> "name" `JSON.kvObjectOrEmpty` n
assetJSONField a "excerpts" _ =
  Just . JSON.mapObjects excerptJSON <$> lookupAssetExcerpts a
assetJSONField _ _ _ = return Nothing

assetJSONQuery :: AssetSlot -> JSON.Query -> Handler (JSON.Record (Id Asset) JSON.Series)
assetJSONQuery o q = (assetSlotJSON False o `JSON.foldObjectIntoRec`) <$> JSON.jsonQuery (assetJSONField o) q
-- public restricted should consult volume

assetDownloadName :: Bool -> Bool -> AssetRow -> [T.Text]
assetDownloadName addPrefix trimFormat a =
  let
    assetName' =
        if trimFormat
        -- original uploaded files have the extension embedded in the name
        then fmap (TE.decodeUtf8 . dropFormatExtension (assetFormat a) . TE.encodeUtf8) (assetName a)
        else assetName a
    scrubbedAssetName :: Maybe T.Text
    scrubbedAssetName =
        fmap scrubAssetName assetName'
  in
    if addPrefix
    then T.pack (show $ assetId a) : maybeToList scrubbedAssetName
    else maybeToList scrubbedAssetName

scrubAssetName :: T.Text -> T.Text
scrubAssetName = T.pack . FPW.makeValid . FPP.makeValid . T.unpack

viewAsset :: ActionRoute (API, Id Asset)
viewAsset = action GET (pathAPI </> pathId) $ \(api, i) -> withAuth $ do
  asset <- getAsset False PermissionPUBLIC True i
  case api of
    JSON -> okResponse [] <$> (assetJSONQuery asset =<< peeks Wai.queryString)
    HTML
      | Just s <- assetSlot asset -> peeks $ otherRouteResponse [] (viewAssetSegment False) (api, Just (view asset), slotId s, assetId $ assetRow $ slotAsset asset)
      | otherwise -> return $ okResponse [] $ T.pack $ show $ assetId $ assetRow $ slotAsset asset -- TODO

data AssetTarget
  = AssetTargetVolume Volume
  | AssetTargetSlot Slot
  | AssetTargetAsset AssetSlot

data FileUploadFile
  = FileUploadForm (FileInfo TempFile)
  | FileUploadToken Upload

fileUploadName :: FileUploadFile -> BS.ByteString
fileUploadName (FileUploadForm f) = fileName f
fileUploadName (FileUploadToken u) = uploadFilename u

fileUploadPath :: FileUploadFile -> Storage -> RawFilePath
fileUploadPath (FileUploadForm f) _ = tempFilePath $ fileContent f
fileUploadPath (FileUploadToken u) s = uploadFile u s

fileUploadRemove :: FileUploadFile -> Handler ()
fileUploadRemove (FileUploadForm f) = focusIO $ releaseTempFile $ fileContent f
fileUploadRemove (FileUploadToken u) = void $ removeUpload u

data FileUpload = FileUpload
  { fileUploadFile :: FileUploadFile
  , fileUploadProbe :: Probe
  }

deformLookup :: (Monad m, Deform f a) => FormErrorMessage -> (a -> m (Maybe b)) -> DeformT f m (Maybe b)
deformLookup e l = mapM (deformMaybe' e <=< lift . l) =<< deformNonEmpty deform

detectUpload :: (MonadHas AV c m, MonadStorage c m) => FileUploadFile -> DeformT TempFile m FileUpload
detectUpload u = do
  liftIO $ print "detectUpload..."
  either deformError' (return . FileUpload u)
    =<< lift (probeFile (fileUploadName u) =<< peeks (fileUploadPath u))

data ProcessAssetRequest =
    ProcessAssetRequest
        (Maybe (FileInfo TempFile))
        (Maybe (Id Token))
        (Maybe T.Text)
        (Maybe Release)
        (Maybe (Id Container))
        ()

processAsset :: AssetTarget -> Handler Response
processAsset target = do
  let as@AssetSlot{ slotAsset = a, assetSlot = s } = case target of
        AssetTargetVolume t -> assetNoSlot $ blankAsset t
        AssetTargetSlot t -> AssetSlot (blankAsset (view t)) (Just t)
        AssetTargetAsset t -> t
  (as', up') <- runFormFiles [("file", maxAssetSize)] (Nothing :: Maybe (RequestContext -> FormHtml a)) $ do
    liftIO $ putStrLn "runFormFiles..."--DEBUG
    csrfForm
    (file :: Maybe (FileInfo TempFile)) <- "file" .:> deform
    liftIO $ putStrLn "deformed file..." --DEBUG
    upload <- "upload" .:> deformLookup "Uploaded file not found." lookupUpload
    liftIO $ putStrLn "upload file..." --DEBUG
    upfile <- case (file, upload) of
      (Just f, Nothing) -> return $ Just $ FileUploadForm f
      (Nothing, Just u) -> return $ Just $ FileUploadToken u
      (Nothing, Nothing)
        | AssetTargetAsset _ <- target -> return Nothing
        | otherwise -> Nothing <$ deformError "File or upload required."
      _ -> Nothing <$ deformError "Conflicting uploaded files found."
    up <- mapM detectUpload upfile
    liftIO $ putStrLn "upfile cased..." --DEBUG
    let fmt = maybe (assetFormat $ assetRow a) (probeFormat . fileUploadProbe) up
    liftIO $ putStrLn "format upload probe..." --DEBUG
    name <- "name" .:> maybe (assetName $ assetRow a) (TE.decodeUtf8 . dropFormatExtension fmt <$>) <$> deformOptional (deformNonEmpty deform)
    liftIO $ putStrLn "renamed asset..." --DEBUG
    classification <- "classification" .:> fromMaybe (assetRelease $ assetRow a) <$> deformOptional (deformNonEmpty deform)
    liftIO $ putStrLn "classification deformed..." --DEBUG
    slot <-
      "container" .:> (<|> slotContainer <$> s) <$> deformLookup "Container not found." (lookupVolumeContainer (assetVolume a))
      >>= mapM (\c -> "position" .:> do
        let seg = slotSegment <$> s
            dur = maybe (assetDuration $ assetRow a) (probeLength . fileUploadProbe) up
        p <- fromMaybe (lowerBound . segmentRange =<< seg) <$> deformOptional (deformNonEmpty deform)
        Slot c . maybe fullSegment
          (\l -> Segment $ Range.bounded l (l + fromMaybe 0 ((segmentLength =<< seg) <|> dur)))
          <$> orElseM p (mapM (lift . probeAutoPosition c . Just . fileUploadProbe) (guard (isNothing s && isJust dur) >> up)))
    let _ =
          ProcessAssetRequest
              file
              (fmap (tokenId . accountToken . uploadAccountToken) upload)
              name
              classification
              (fmap (containerId . containerRow . slotContainer) slot)
              () -- TODO: populate with parsed position value
    liftIO $ putStrLn "slot assigned..." --DEBUG
    return
      ( as
        { slotAsset = a
          { assetRow = (assetRow a)
            { assetName = name
            , assetRelease = classification
            , assetFormat = fmt
            }
          }
        , assetSlot = slot
        }
      , up
      )
  as'' <- maybe (return as') (\up@FileUpload{ fileUploadFile = upfile } -> do
    a' <- addAsset (slotAsset as')
      { assetRow = (assetRow $ slotAsset as')
        { assetName = Just $ TE.decodeUtf8 $ fileUploadName upfile
        , assetDuration = Nothing
        , assetSize = Nothing
        , assetSHA1 = Nothing
        }
      } . Just =<< peeks (fileUploadPath upfile)
    fileUploadRemove upfile
    td <- checkAlreadyTranscoded a' (fileUploadProbe up)
    te <- peeks transcodeEnabled
    t <- case fileUploadProbe up of
      ProbeAV{ probeAV = av } | td ->
        return a'{ assetRow = (assetRow a'){ assetDuration = avProbeLength av } }
      probe@ProbeAV{} | te -> do
        t <- addTranscode a' fullSegment defaultTranscodeOptions probe
        _ <- forkTranscode t
        return $ transcodeAsset t
      _ -> return a'
    case target of
      AssetTargetAsset _ -> replaceAsset a t
      _ -> return ()
    return $ fixAssetSlotDuration as'
      { slotAsset = t
        { assetRow = (assetRow t)
          { assetName = assetName $ assetRow $ slotAsset as'
          }
        }
      })
    up'
  a' <- changeAsset (slotAsset as'') Nothing
  liftIO $ putStrLn "changed asset..." --DEBUG
  _ <- changeAssetSlot as''
  liftIO $ putStrLn "change asset slot..." --DEBUG
  when (assetRelease (assetRow a') == Just ReleasePUBLIC && assetRelease (assetRow a) /= Just ReleasePUBLIC) $
    createVolumeNotification (assetVolume a') $ \n -> (n NoticeReleaseAsset)
      { notificationContainerId = containerId . containerRow . slotContainer <$> assetSlot as''
      , notificationSegment = slotSegment <$> assetSlot as''
      , notificationAssetId = Just $ assetId $ assetRow a'
      , notificationRelease = assetRelease $ assetRow a'
      }
  do
      liftIO $ putStrLn "JSON ok response..." --DEBUG
      return $ okResponse [] $ JSON.recordEncoding $ assetSlotJSON False as'' -- publicrestrict false because EDIT
  -- HTML -> do
  --    liftIO $ putStrLn "returning HTML other route reponse..." --DEBUG
  --    peeks $ otherRouteResponse [] viewAsset (api, assetId $ assetRow $ slotAsset as'')

postAsset :: ActionRoute (Id Asset)
postAsset = multipartAction $ action POST (pathJSON >/> pathId) $ \ai -> withAuth $ do
  asset <- getAsset False PermissionEDIT False ai
  r <- assetIsReplaced (slotAsset asset)
  when r $ result $
    response conflict409 [] ("This file has already been replaced." :: T.Text)
  processAsset $ AssetTargetAsset asset

{-
viewAssetEdit :: ActionRoute (Id Asset)
viewAssetEdit = action GET (pathHTML >/> pathId </< "edit") $ \ai -> withAuth $ do
  asset <- getAsset False PermissionEDIT False ai
  peeks $ blankForm . htmlAssetEdit (AssetTargetAsset asset)
-}

createAsset :: ActionRoute (Id Volume)
createAsset = multipartAction $ action POST (pathJSON >/> pathId </< "asset") $ \vi -> withAuth $ do
  liftIO $ print "getting volume permission..."
  v <- getVolume PermissionEDIT vi
  liftIO $ print "processing asset..."
  processAsset $ AssetTargetVolume v

{-
viewAssetCreate :: ActionRoute (Id Volume)
viewAssetCreate = action GET (pathHTML >/> pathId </< "asset") $ \vi -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  peeks $ blankForm . htmlAssetEdit (AssetTargetVolume v)
-}

createSlotAsset :: ActionRoute (Id Slot)
createSlotAsset = multipartAction $ action POST (pathJSON >/> pathSlotId </< "asset") $ \si -> withAuth $ do
  v <- getSlot PermissionEDIT Nothing si
  processAsset $ AssetTargetSlot v

{-
viewSlotAssetCreate :: ActionRoute (Id Slot)
viewSlotAssetCreate = action GET (pathHTML >/> pathSlotId </< "asset") $ \si -> withAuth $ do
  s <- getSlot PermissionEDIT Nothing si
  peeks $ blankForm . htmlAssetEdit (AssetTargetSlot s)
-}

deleteAsset :: ActionRoute (Id Asset)
deleteAsset = action DELETE (pathJSON >/> pathId) $ \ai -> withAuth $ do
  guardVerfHeader
  asset <- getAsset False PermissionEDIT False ai
  let asset' = asset{ assetSlot = Nothing }
  _ <- changeAssetSlot asset'
  return $ okResponse [] $ JSON.recordEncoding $ assetSlotJSON False asset' -- publicRestricted false because EDIT
  --  HTML -> peeks $ otherRouteResponse [] viewAsset (api, assetId $ assetRow $ slotAsset asset')

downloadAsset :: ActionRoute (Id Asset, Segment)
downloadAsset = action GET (pathId </> pathSegment </< "download") $ \(ai, seg) -> withAuth $ do
  a <- getAsset False PermissionPUBLIC True ai
  inline <- peeks $ lookupQueryParameters "inline"
  serveAssetSegment (null inline) $ newAssetSegment a seg Nothing

downloadOrigAsset :: ActionRoute (Id Asset, Segment)
downloadOrigAsset = action GET (pathId </> pathSegment </< "downloadOrig") $ \(ai, seg) -> withAuth $ do
  a <- getAsset True PermissionPUBLIC True ai
  inline <- peeks $ lookupQueryParameters "inline"
  serveAssetSegment (null inline) $ newAssetSegment a seg Nothing

thumbAsset :: ActionRoute (Id Asset, Segment)
thumbAsset = action GET (pathId </> pathSegment </< "thumb") $ \(ai, seg) -> withAuth $ do
  a <- getAsset False PermissionPUBLIC False ai
  let as = assetSegmentInterp 0.25 $ newAssetSegment a seg Nothing
  if formatIsImage (view as)
    && assetBacked (view as)
    && canReadData2 getAssetSegmentRelease2 getAssetSegmentVolumePermission2 as
    then peeks $ otherRouteResponse [] downloadAsset (view as, assetSegment as)
    else peeks $ otherRouteResponse [] formatIcon (view as)
