{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Controller.AssetSegment
  ( getAssetSegment
  , viewAssetSegment
  , serveAssetSegment
  , downloadAssetSegment
  , downloadOrigAssetSegment
  , thumbAssetSegment
  ) where

import Control.Monad ((<=<), join, when, mfilter, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isJust, fromJust, listToMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Network.HTTP.Types (ResponseHeaders)
import Network.HTTP.Types.Status (movedPermanently301)
import qualified Network.Wai as Wai
import Text.Read (readMaybe)
import System.Posix.Types (FileOffset)

import Files (unRawFilePath, RawFilePath)
import Ops
import Has (view, peeks)
import qualified JSON
import Files (fileInfo)
import Model.Id
import Model.Permission hiding (checkPermission)
import Model.Volume
import Model.Slot
import Model.Format
import Model.Asset
import Model.AssetSlot
import Model.AssetSegment
import Store.Asset
import Store.AssetSegment
import Store.Filename
import HTTP.File
import HTTP.Request
import HTTP.Path.Parser
import Action
import Controller.Paths
import Controller.Angular
import Controller.Permission
import Controller.Volume
import Controller.Slot
import Controller.Asset
import Controller.Format

-- Boolean flag to toggle the choice of downloading the original asset file.
getAssetSegment :: Bool -> Permission -> Bool -> Maybe (Id Volume) -> Id Slot -> Id Asset -> Handler AssetSegment
getAssetSegment getOrig p checkDataPerm mv s a = do
  mAssetSeg <- (if getOrig then lookupOrigSlotAssetSegment else lookupSlotAssetSegment) s a
  assetSeg <- maybeAction (maybe id (\v -> mfilter $ (v ==) . view) mv mAssetSeg)
  void (checkPermission (extractPermissionIgnorePolicy . getAssetSegmentVolumePermission2) p assetSeg)
  when checkDataPerm $
    void (userCanReadData getAssetSegmentRelease2 getAssetSegmentVolumePermission2 assetSeg)
  pure assetSeg

assetSegmentJSONField :: AssetSegment -> BS.ByteString -> Maybe BS.ByteString -> Handler (Maybe JSON.Encoding)
assetSegmentJSONField a "asset" _ = return $ Just $ JSON.recordEncoding $ assetSlotJSON False (segmentAsset a)
assetSegmentJSONField a v o = assetJSONField (segmentAsset a) v o
-- publicRestricted should consult volume

assetSegmentJSONQuery :: AssetSegment -> JSON.Query -> Handler JSON.Series
assetSegmentJSONQuery o q = (assetSegmentJSON o <>) <$> JSON.jsonQuery (assetSegmentJSONField o) q

assetSegmentDownloadName :: AssetSegment -> [T.Text]
assetSegmentDownloadName a =
     volumeDownloadName (view a)
  ++ foldMap slotDownloadName (assetSlot $ segmentAsset a)
  ++ assetDownloadName True False (assetRow $ view a)

viewAssetSegment :: Bool -> ActionRoute (API, Maybe (Id Volume), Id Slot, Id Asset)
viewAssetSegment getOrig = action GET (pathAPI </>>> pathMaybe pathId </>> pathSlotId </> pathId) $ \(api, vi, si, ai) -> withAuth $ do
  when (api == HTML && isJust vi) angular
  as <- getAssetSegment getOrig PermissionPUBLIC True vi si ai
  case api of
    JSON -> okResponse [] <$> (assetSegmentJSONQuery as =<< peeks Wai.queryString)
    HTML
      | isJust vi -> return $ okResponse [] $ T.pack $ show $ assetId $ assetRow $ slotAsset $ segmentAsset as
      | otherwise -> peeks $ redirectRouteResponse movedPermanently301 [] (viewAssetSegment getOrig) (api, Just (view as), slotId $ view as, view as)

serveAssetSegment :: Bool -> AssetSegment -> Handler Response
serveAssetSegment dl as = do
  -- liftIO $ print ("download?", dl)
  -- liftIO $ print ("asset seg?", as)
  sz <- peeks $ readMaybe . BSC.unpack <=< join . listToMaybe . lookupQueryParameters "size"
  -- liftIO $ print ("determined size", sz)
  when dl $ auditAssetSegmentDownload True as
  store :: RawFilePath <- maybeAction =<< getAssetFile a
  (hd :: ResponseHeaders, part :: Maybe FileOffset) <-
    fileResponse
      store
      (view as :: Format)
      (dl `thenUse` makeFilename (assetSegmentDownloadName as) :: Maybe BS.ByteString) -- download file name
      (BSL.toStrict $ BSB.toLazyByteString $  -- etag for http serve
        BSB.byteStringHex (fromJust $ assetSHA1 $ assetRow a) <> BSB.string8 (assetSegmentTag as sz)
        :: BS.ByteString)
  (eStreamRunnerOrFile :: Either ((BS.ByteString -> IO ()) -> IO ()) RawFilePath) <- getAssetSegmentStore as sz
  either
    (return . okResponse hd)
    (\f -> do
      Just (z, _) <- liftIO $ fileInfo f
      fp <- liftIO $ unRawFilePath f
      return $ okResponse hd (fp, z <$ part))
    eStreamRunnerOrFile
  where
  a = slotAsset $ segmentAsset as

downloadAssetSegment :: ActionRoute (Id Slot, Id Asset)
downloadAssetSegment = action GET (pathSlotId </> pathId </< "download") $ \(si, ai) -> withAuth $ do
  as <- getAssetSegment False PermissionPUBLIC True Nothing si ai
  inline <- peeks $ boolQueryParameter "inline"
  serveAssetSegment (not inline) as

downloadOrigAssetSegment :: ActionRoute (Id Slot, Id Asset)
downloadOrigAssetSegment = action GET (pathSlotId </> pathId </< "downloadOrig") $ \(si, ai) -> withAuth $ do
  as <- getAssetSegment True PermissionPUBLIC True Nothing si ai
  inline <- peeks $ boolQueryParameter "inline"
  serveAssetSegment (not inline) as


thumbAssetSegment :: Bool -> ActionRoute (Id Slot, Id Asset)
thumbAssetSegment getOrig = action GET (pathSlotId </> pathId </< "thumb") $ \(si, ai) -> withAuth $ do
  as <- getAssetSegment getOrig PermissionPUBLIC False Nothing si ai  -- why checkDataPerm == False?
  let as' = assetSegmentInterp 0.25 as
  if formatIsImage (view as')
    && assetBacked (view as)
    && canReadData2 getAssetSegmentRelease2 getAssetSegmentVolumePermission2 as'
    then peeks $ otherRouteResponse [] downloadAssetSegment (slotId $ view as', assetId $ assetRow $ view as')
    else peeks $ otherRouteResponse [] formatIcon (view as)
