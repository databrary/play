{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds #-}
module Databrary.Model.AssetSlot
  ( module Databrary.Model.AssetSlot.Types
  , lookupAssetSlot
  , lookupOrigAssetSlot 
  , lookupAssetAssetSlot
  , lookupSlotAssets
  , lookupOrigSlotAssets
  , lookupContainerAssets
  , lookupOrigContainerAssets
  , lookupVolumeAssetSlots
  , lookupOrigVolumeAssetSlots
  , lookupOrigVolumeAssetSlots'
  , lookupVolumeAssetSlotIds
  , lookupOrigVolumeAssetSlotIds
  , changeAssetSlot
  , changeAssetSlotDuration
  , fixAssetSlotDuration
  , findAssetContainerEnd
  , assetSlotName
  , assetSlotJSON
  ) where

import Control.Monad (when, guard)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Text as T
import Database.PostgreSQL.Typed (pgSQL)

import Databrary.Ops
import Databrary.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Offset
import Databrary.Model.Permission
import Databrary.Model.Segment
import Databrary.Model.Id
import Databrary.Model.Party.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Asset
import Databrary.Model.Audit
import Databrary.Model.SQL
import Databrary.Model.AssetSlot.Types
import Databrary.Model.AssetSlot.SQL
import Databrary.Model.Format.Types
import Databrary.Model.PermissionUtil (maskRestrictedString)

lookupAssetSlot :: (MonadHasIdentity c m, MonadDB c m) => Id Asset -> m (Maybe AssetSlot)
lookupAssetSlot ai = do
  ident <- peek
  dbQuery1 $(selectQuery (selectAssetSlot 'ident) "$WHERE asset.id = ${ai}")

lookupOrigAssetSlot :: (MonadHasIdentity c m, MonadDB c m) => Id Asset -> m (Maybe AssetSlot)
lookupOrigAssetSlot ai = do
  initAsset <- lookupAssetSlot ai
  let format = formatName . assetFormat . assetRow . slotAsset $ fromJust initAsset
  case format of 
    ".pdf" -> lookupAssetSlot ai --TODO format name should support all doc types
    _ -> do 
      ident <- peek
      dbQuery1 $(selectQuery (selectAssetSlot 'ident) "$left join transcode tc on tc.orig = asset.id WHERE tc.asset = ${ai}")

lookupAssetAssetSlot :: (MonadDB c m) => Asset -> m AssetSlot
lookupAssetAssetSlot a = fromMaybe assetNoSlot
  <$> dbQuery1 $(selectQuery selectAssetSlotAsset "$WHERE slot_asset.asset = ${assetId $ assetRow a} AND container.volume = ${volumeId $ volumeRow $ assetVolume a}")
  <*> return a

lookupSlotAssets :: (MonadDB c m) => Slot -> m [AssetSlot]
lookupSlotAssets (Slot c s) =
  dbQuery $ ($ c) <$> $(selectQuery selectContainerSlotAsset "$WHERE slot_asset.container = ${containerId $ containerRow c} AND slot_asset.segment && ${s} AND asset.volume = ${volumeId $ volumeRow $ containerVolume c}")

lookupOrigSlotAssets :: (MonadDB c m) => Slot -> m [AssetSlot]
lookupOrigSlotAssets slot@(Slot c _) = do
  xs <-  dbQuery [pgSQL|
    SELECT asset.id,asset.release,asset.duration,asset.name,asset.sha1,asset.size 
    FROM slot_asset 
    INNER JOIN asset_revision ON slot_asset.asset = asset_revision.asset
    INNER JOIN asset ON asset_revision.orig = asset.id
    WHERE slot_asset.container = ${containerId $ containerRow c}
    |]
  return $ flip fmap xs $ \(assetId,release,duration,name,sha1,size) -> 
    let format = Format (Id (-800)) "video/mp4" [] "" {-fromJust . getFormatByExtension $ encodeUtf8 $ fromJust name-} 
        assetRow = AssetRow (Id assetId) format release duration name sha1 size
    in AssetSlot (Asset assetRow (containerVolume c)) (Just slot)

lookupContainerAssets :: (MonadDB c m) => Container -> m [AssetSlot]
lookupContainerAssets = lookupSlotAssets . containerSlot

lookupOrigContainerAssets :: (MonadDB c m) => Container -> m [AssetSlot]
lookupOrigContainerAssets = lookupOrigSlotAssets . containerSlot

lookupVolumeAssetSlots :: (MonadDB c m) => Volume -> Bool -> m [AssetSlot]
lookupVolumeAssetSlots v top =
  dbQuery $ ($ v) <$> $(selectQuery selectVolumeSlotAsset "$WHERE asset.volume = ${volumeId $ volumeRow v} AND (container.top OR ${not top}) ORDER BY container.id")

lookupOrigVolumeAssetSlots :: (MonadDB c m, MonadHasIdentity c m) => Volume -> Bool -> m [AssetSlot]
lookupOrigVolumeAssetSlots v top = do
  fromVol <- lookupVolumeAssetSlots v top
  lookupOrigVolumeAssetSlots' fromVol

lookupOrigVolumeAssetSlots' :: (MonadDB c m, MonadHasIdentity c m) => [AssetSlot] -> m [AssetSlot]
lookupOrigVolumeAssetSlots' slotList = do
  catMaybes <$> mapM originFinder slotList
  where 
    originFinder (AssetSlot { slotAsset = Asset {assetRow = AssetRow { assetId = aid }}}) = lookupOrigAssetSlot aid

lookupVolumeAssetSlotIds :: (MonadDB c m) => Volume -> m [(Asset, SlotId)]
lookupVolumeAssetSlotIds v =
  dbQuery $ ($ v) <$> $(selectQuery selectVolumeSlotIdAsset "$WHERE asset.volume = ${volumeId $ volumeRow v} ORDER BY container")

lookupOrigVolumeAssetSlotIds :: (MonadDB c m) => Volume -> m [(Asset, SlotId)]
lookupOrigVolumeAssetSlotIds v =
  dbQuery $ ($ v) <$> $(selectQuery selectVolumeSlotIdAsset "$left join asset_revision ar on ar.orig = asset.id WHERE asset.volume = ${volumeId $ volumeRow v} ORDER BY container")

changeAssetSlot :: (MonadAudit c m) => AssetSlot -> m Bool
changeAssetSlot as = do
  ident <- getAuditIdentity
  if isNothing (assetSlot as)
    then dbExecute1 $(deleteSlotAsset 'ident 'as)
    else do
      (r, _) <- updateOrInsert
        $(updateSlotAsset 'ident 'as)
        $(insertSlotAsset 'ident 'as)
      when (r /= 1) $ fail $ "changeAssetSlot: " ++ show r ++ " rows"
      return True

changeAssetSlotDuration :: MonadDB c m => Asset -> m Bool
changeAssetSlotDuration a
  | Just dur <- assetDuration $ assetRow a =
    dbExecute1 [pgSQL|UPDATE slot_asset SET segment = segment(lower(segment), lower(segment) + ${dur}) WHERE asset = ${assetId $ assetRow a}|]
  | otherwise = return False

fixAssetSlotDuration :: AssetSlot -> AssetSlot
fixAssetSlotDuration as
  | Just dur <- assetDuration $ assetRow $ slotAsset as = as{ assetSlot = (\s -> s{ slotSegment = segmentSetDuration dur (slotSegment s) }) <$> assetSlot as }
  | otherwise = as

findAssetContainerEnd :: MonadDB c m => Container -> m Offset
findAssetContainerEnd c = fromMaybe 0 <$>
  dbQuery1' [pgSQL|SELECT max(upper(segment))+'1s' FROM slot_asset WHERE container = ${containerId $ containerRow c}|]

assetSlotName :: AssetSlot -> Maybe T.Text
assetSlotName a = guard (any (containerTop . containerRow . slotContainer) (assetSlot a) || dataPermission a > PermissionNONE) >> assetName (assetRow $ slotAsset a)

assetSlotJSON :: JSON.ToObject o => Bool -> AssetSlot -> JSON.Record (Id Asset) o
assetSlotJSON publicRestricted as@AssetSlot{..} = assetJSON publicRestricted slotAsset JSON..<>
  foldMap (segmentJSON . slotSegment) assetSlot
  --  "release" JSON..=? (view as :: Maybe Release)
  <> "name" JSON..=? (if publicRestricted then fmap maskRestrictedString (assetSlotName as) else assetSlotName as)
  <> "permission" JSON..= p
  <> "size" JSON..=? (z <? p > PermissionNONE && any (0 <=) z)
  where
  p = dataPermission as
  z = assetSize $ assetRow slotAsset

{-
assetSlotJSONRestricted :: JSON.ToObject o => AssetSlot -> JSON.Record (Id Asset) o
assetSlotJSONRestricted as@AssetSlot{..} = assetJSONRestricted slotAsset JSON..<>
  foldMap (segmentJSON . slotSegment) assetSlot
  --  "release" JSON..=? (view as :: Maybe Release)
  <> "name" JSON..=? (fmap maskRestrictedString (assetSlotName as))
  <> "permission" JSON..= p
  <> "size" JSON..=? (z <? p > PermissionNONE && any (0 <=) z)
  where
  p = dataPermission as
  z = assetSize $ assetRow slotAsset
-}
