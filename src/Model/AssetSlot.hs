{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds #-}
module Model.AssetSlot
  ( module Model.AssetSlot.Types
  , lookupAssetSlot
  , lookupOrigAssetSlot
  , lookupAssetAssetSlot
  , lookupSlotAssets
  , lookupOrigSlotAssets
  , lookupContainerAssets
  , lookupOrigContainerAssets
  , lookupVolumeAssetSlots
  -- , lookupOrigVolumeAssetSlots
  , lookupOrigVolumeAssetSlots'
  , lookupVolumeAssetSlotIds
  -- , lookupOrigVolumeAssetSlotIds
  , changeAssetSlot
  , changeAssetSlotDuration
  , fixAssetSlotDuration
  , findAssetContainerEnd
  , assetSlotName
  , assetSlotJSON
  ) where

import Control.Monad (when, guard)
import qualified Data.ByteString
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import Data.Maybe (fromJust, catMaybes)
import Data.String
import qualified Data.Text as T
-- import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Types

import Ops
import Has (peek, view)
import qualified JSON as JSON
import Service.DB
import Model.Offset
import Model.Permission
import Model.Segment
import Model.Id
import Model.Party.Types
import Model.Identity.Types
import Model.Volume.Types
import Model.Container.Types
import Model.Slot.Types
import Model.Asset
import Model.Audit
import Model.SQL
import Model.AssetSlot.Types
import Model.AssetSlot.SQL
import Model.Format.Types
import Model.Format (getFormat')
import Model.PermissionUtil (maskRestrictedString)

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
  let _tenv_ablno = unknownPGTypeEnv
  xs <-  dbQuery {- [pgSQL|
    SELECT asset.id,asset.format,output_asset.release,asset.duration,asset.name,asset.sha1,asset.size 
    FROM slot_asset 
    INNER JOIN transcode ON slot_asset.asset = transcode.asset
    INNER JOIN asset ON transcode.orig = asset.id
    INNER JOIN asset output_asset ON transcode.asset = output_asset.id
    WHERE slot_asset.container = ${containerId $ containerRow c}
    |] -}
   (mapQuery2
    ((\ _p_ablnp ->
                    Data.ByteString.concat
                       [fromString
                          "\n\
                          \    SELECT asset.id,asset.format,output_asset.release,asset.duration,asset.name,asset.sha1,asset.size \n\
                          \    FROM slot_asset \n\
                          \    INNER JOIN transcode ON slot_asset.asset = transcode.asset\n\
                          \    INNER JOIN asset ON transcode.orig = asset.id\n\
                          \    INNER JOIN asset output_asset ON transcode.asset = output_asset.id\n\
                          \    WHERE slot_asset.container = ",
                        pgEscapeParameter
                          _tenv_ablno (PGTypeProxy :: PGTypeName "integer") _p_ablnp,
                        fromString
                          "\n\
                          \    "])
     (containerId $ containerRow c))
            (\
               [_cid_ablnq,
                _cformat_ablnr,
                _crelease_ablns,
                _cduration_ablnt,
                _cname_ablnu,
                _csha1_ablnv,
                _csize_ablnw]
               -> (pgDecodeColumnNotNull
                     _tenv_ablno (PGTypeProxy :: PGTypeName "integer") _cid_ablnq,
                   pgDecodeColumnNotNull
                     _tenv_ablno (PGTypeProxy :: PGTypeName "smallint") _cformat_ablnr,
                   pgDecodeColumn
                     _tenv_ablno (PGTypeProxy :: PGTypeName "release") _crelease_ablns,
                   pgDecodeColumn
                     _tenv_ablno
                     (PGTypeProxy :: PGTypeName "interval")
                     _cduration_ablnt,
                   pgDecodeColumn
                     _tenv_ablno (PGTypeProxy :: PGTypeName "text") _cname_ablnu,
                   pgDecodeColumn
                     _tenv_ablno (PGTypeProxy :: PGTypeName "bytea") _csha1_ablnv,
                   pgDecodeColumn
                     _tenv_ablno (PGTypeProxy :: PGTypeName "bigint") _csize_ablnw)))
  return $ flip fmap xs $ \(assetId,formatId,release,duration,name,sha1,size) ->
    -- this format value is only used to differentiate between audio/video or not
    -- so it is okay that it is hardcoded to mp4, under the assumption that everything with an original
    -- was an audio/video file that went through transcoding
    let format = getFormat' formatId
          -- Format (Id (-800)) "video/mp4" [] "" {-fromJust . getFormatByExtension $ encodeUtf8 $ fromJust name-}
        assetRow = AssetRow (Id assetId) format release duration name sha1 size
    in AssetSlot (Asset assetRow (containerVolume c)) (Just slot)

lookupContainerAssets :: (MonadDB c m) => Container -> m [AssetSlot]
lookupContainerAssets = lookupSlotAssets . containerSlot

lookupOrigContainerAssets :: (MonadDB c m) => Container -> m [AssetSlot]
lookupOrigContainerAssets = lookupOrigSlotAssets . containerSlot

lookupVolumeAssetSlots :: (MonadDB c m) => Volume -> Bool -> m [AssetSlot]
lookupVolumeAssetSlots v top =
  dbQuery $ ($ v) <$> $(selectQuery selectVolumeSlotAsset "$WHERE asset.volume = ${volumeId $ volumeRow v} AND (container.top OR ${not top}) ORDER BY container.id")

{- lookupOrigVolumeAssetSlots :: (MonadDB c m, MonadHasIdentity c m) => Volume -> Bool -> m [AssetSlot]
lookupOrigVolumeAssetSlots v top = do
  fromVol <- lookupVolumeAssetSlots v top
  lookupOrigVolumeAssetSlots' fromVol -}

lookupOrigVolumeAssetSlots' :: (MonadDB c m, MonadHasIdentity c m) => [AssetSlot] -> m [AssetSlot]
lookupOrigVolumeAssetSlots' slotList = do
  catMaybes <$> mapM originFinder slotList
  where
    originFinder AssetSlot { slotAsset = Asset {assetRow = AssetRow { assetId = aid }}} = lookupOrigAssetSlot aid

lookupVolumeAssetSlotIds :: (MonadDB c m) => Volume -> m [(Asset, SlotId)]
lookupVolumeAssetSlotIds v =
  dbQuery $ ($ v) <$> $(selectQuery selectVolumeSlotIdAsset "$WHERE asset.volume = ${volumeId $ volumeRow v} ORDER BY container")

{- lookupOrigVolumeAssetSlotIds :: (MonadDB c m) => Volume -> m [(Asset, SlotId)]
lookupOrigVolumeAssetSlotIds v =
  dbQuery $ ($ v) <$> $(selectQuery selectVolumeSlotIdAsset "$left join asset_revision ar on ar.orig = asset.id WHERE asset.volume = ${volumeId $ volumeRow v} ORDER BY container") -}

changeAssetSlot :: (MonadAudit c m) => AssetSlot -> m Bool
changeAssetSlot as = do
  ident <- getAuditIdentity
  let _tenv_a8II3 = unknownPGTypeEnv
  if isNothing (assetSlot as)
    then dbExecute1 -- (deleteSlotAsset 'ident 'as)
      (mapQuery2
          ((\ _p_a8II4 _p_a8II5 _p_a8II6 ->
                          (Data.ByteString.concat
                             [Data.String.fromString
                                "WITH audit_row AS (DELETE FROM slot_asset WHERE asset=",
                              Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                _tenv_a8II3
                                (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                   Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                                _p_a8II4,
                              Data.String.fromString
                                " RETURNING *) INSERT INTO audit.slot_asset SELECT CURRENT_TIMESTAMP, ",
                              Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                _tenv_a8II3
                                (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                   Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                                _p_a8II5,
                              Data.String.fromString ", ",
                              Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                _tenv_a8II3
                                (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                   Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                                _p_a8II6,
                              Data.String.fromString
                                ", 'remove'::audit.action, * FROM audit_row"]))
              (assetId $ assetRow $ slotAsset as)
              (auditWho ident)
              (auditIp ident))
          (\ [] -> ()))
    else do
      let _tenv_a8IMD = unknownPGTypeEnv
          _tenv_a8IPn = unknownPGTypeEnv
      (r, _) <- updateOrInsert
        -- (updateSlotAsset 'ident 'as)
        (mapQuery2
           ((\ _p_a8IME _p_a8IMF _p_a8IMG _p_a8IMH _p_a8IMI ->
                           (Data.ByteString.concat
                              [Data.String.fromString
                                 "WITH audit_row AS (UPDATE slot_asset SET container=",
                               Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                 _tenv_a8IMD
                                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                    Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                                 _p_a8IME,
                               Data.String.fromString ",segment=",
                               Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                 _tenv_a8IMD
                                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                    Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                                 _p_a8IMF,
                               Data.String.fromString " WHERE asset=",
                               Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                 _tenv_a8IMD
                                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                    Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                                 _p_a8IMG,
                               Data.String.fromString
                                 " RETURNING *) INSERT INTO audit.slot_asset SELECT CURRENT_TIMESTAMP, ",
                               Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                 _tenv_a8IMD
                                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                    Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                                 _p_a8IMH,
                               Data.String.fromString ", ",
                               Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                 _tenv_a8IMD
                                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                    Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                                 _p_a8IMI,
                               Data.String.fromString
                                 ", 'change'::audit.action, * FROM audit_row"]))
             (containerId . containerRow . slotContainer <$> assetSlot as)
             (slotSegment <$> assetSlot as)
             (assetId $ assetRow $ slotAsset as)
             (auditWho ident)
             (auditIp ident))
            (\[] -> ()))
        -- (insertSlotAsset 'ident 'as)
        (mapQuery2
          ((\ _p_a8IPo _p_a8IPp _p_a8IPq _p_a8IPr _p_a8IPs ->
                          (Data.ByteString.concat
                             [Data.String.fromString
                                "WITH audit_row AS (INSERT INTO slot_asset (asset,container,segment) VALUES (",
                              Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                _tenv_a8IPn
                                (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                   Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                                _p_a8IPo,
                              Data.String.fromString ",",
                              Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                _tenv_a8IPn
                                (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                   Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                                _p_a8IPp,
                              Data.String.fromString ",",
                              Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                _tenv_a8IPn
                                (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                   Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                                _p_a8IPq,
                              Data.String.fromString
                                ") RETURNING *) INSERT INTO audit.slot_asset SELECT CURRENT_TIMESTAMP, ",
                              Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                _tenv_a8IPn
                                (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                   Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                                _p_a8IPr,
                              Data.String.fromString ", ",
                              Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                _tenv_a8IPn
                                (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                   Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                                _p_a8IPs,
                              Data.String.fromString ", 'add'::audit.action, * FROM audit_row"]))
                (assetId $ assetRow $ slotAsset as)
                (containerId . containerRow . slotContainer <$> assetSlot as)
                (slotSegment <$> assetSlot as)
                (auditWho ident)
                (auditIp ident))
            (\[] -> ()))
      when (r /= 1) $ fail $ "changeAssetSlot: " ++ show r ++ " rows"
      return True

changeAssetSlotDuration :: MonadDB c m => Asset -> m Bool
changeAssetSlotDuration a
  | Just dur <- assetDuration $ assetRow a = do
     let _tenv_ablLj = unknownPGTypeEnv
     dbExecute1 -- [pgSQL|UPDATE slot_asset SET segment = segment(lower(segment), lower(segment) + ${dur}) WHERE asset = ${assetId $ assetRow a}|]
      (mapQuery2
        ((\ _p_ablLk _p_ablLl ->
                        (Data.ByteString.concat
                           [fromString
                              "UPDATE slot_asset SET segment = segment(lower(segment), lower(segment) + ",
                            pgEscapeParameter
                              _tenv_ablLj (PGTypeProxy :: PGTypeName "interval") _p_ablLk,
                            fromString ") WHERE asset = ",
                            pgEscapeParameter
                              _tenv_ablLj (PGTypeProxy :: PGTypeName "integer") _p_ablLl]))
          dur (assetId $ assetRow a))
        (\[] -> ()))
  | otherwise = return False

fixAssetSlotDuration :: AssetSlot -> AssetSlot
fixAssetSlotDuration as
  | Just dur <- assetDuration $ assetRow $ slotAsset as = as{ assetSlot = (\s -> s{ slotSegment = segmentSetDuration dur (slotSegment s) }) <$> assetSlot as }
  | otherwise = as

findAssetContainerEnd :: MonadDB c m => Container -> m Offset
findAssetContainerEnd c = do
  let _tenv_ablQT = unknownPGTypeEnv
  fromMaybe 0 <$>
    dbQuery1' -- [pgSQL|SELECT max(upper(segment))+'1s' FROM slot_asset WHERE container = ${containerId $ containerRow c}|]
     (mapQuery2
      ((\ _p_ablQU ->
                      Data.ByteString.concat
                         [fromString
                            "SELECT max(upper(segment))+'1s' FROM slot_asset WHERE container = ",
                          pgEscapeParameter
                            _tenv_ablQT (PGTypeProxy :: PGTypeName "integer") _p_ablQU])
        (containerId $ containerRow c))
              (\[_ccolumn_ablQV]
                 -> (pgDecodeColumn
                       _tenv_ablQT
                       (PGTypeProxy :: PGTypeName "interval")
                       _ccolumn_ablQV)))

assetSlotName :: AssetSlot -> Maybe T.Text
assetSlotName a =
  guard
    (any (containerTop . containerRow . slotContainer) (assetSlot a)
     || canReadData2 getAssetSlotRelease2 getAssetSlotVolumePermission2 a)
  >> assetName (assetRow $ slotAsset a)

assetSlotJSON :: JSON.ToObject o => Bool -> AssetSlot -> JSON.Record (Id Asset) o
assetSlotJSON publicRestricted as@AssetSlot{..} = assetJSON publicRestricted slotAsset `JSON.foldObjectIntoRec`
 (foldMap (segmentJSON . slotSegment) assetSlot
  --  "release" `JSON.kvObjectOrEmpty` (view as :: Maybe Release)
  <> "name" `JSON.kvObjectOrEmpty` (if publicRestricted then fmap maskRestrictedString (assetSlotName as) else assetSlotName as)
  <> "permission" JSON..= p
  <> "size" `JSON.kvObjectOrEmpty` (z `useWhen` (p > PermissionNONE && any (0 <=) z)))
  where
  p = dataPermission4 getAssetSlotRelease2 getAssetSlotVolumePermission2 as
  z = assetSize $ assetRow slotAsset

{-
assetSlotJSONRestricted :: JSON.ToObject o => AssetSlot -> JSON.Record (Id Asset) o
assetSlotJSONRestricted as@AssetSlot{..} = assetJSONRestricted slotAsset JSON..<>
  foldMap (segmentJSON . slotSegment) assetSlot
  --  "release" `JSON.kvObjectOrEmpty` (view as :: Maybe Release)
  <> "name" `JSON.kvObjectOrEmpty` (fmap maskRestrictedString (assetSlotName as))
  <> "permission" JSON..= p
  <> "size" `JSON.kvObjectOrEmpty` (z <? p > PermissionNONE && any (0 <=) z)
  where
  p = dataPermission as
  z = assetSize $ assetRow slotAsset
-}
