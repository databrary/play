{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, OverloadedStrings, ScopedTypeVariables, DataKinds #-}
module Databrary.Model.AssetSegment
  ( module Databrary.Model.AssetSegment.Types
  , lookupAssetSegment
  , lookupSlotAssetSegment
  , lookupAssetSlotSegment
  , lookupOrigSlotAssetSegment
  , lookupSlotSegmentThumb
  , auditAssetSegmentDownload
  , assetSegmentJSON
  , assetSegmentInterp
  ) where

import Control.Applicative (pure, empty)
import Data.Monoid ((<>))
import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Types
import qualified Data.ByteString
import Data.ByteString (ByteString)
import qualified Data.String

import Databrary.Ops
import Databrary.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Audit
import Databrary.Model.Id
import Databrary.Model.Party.Types
import Databrary.Model.Identity
import Databrary.Model.Permission
import Databrary.Model.Segment
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Format.Types
import Databrary.Model.Asset.Types
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment.Types
import Databrary.Model.AssetSegment.SQL
import Databrary.Model.Volume.SQL
import Databrary.Model.Asset.SQL

lookupAssetSegment :: (MonadHasIdentity c m, MonadDB c m) => Segment -> Id Asset -> m (Maybe AssetSegment)
lookupAssetSegment seg ai = do
  let _tenv_acO5F = unknownPGTypeEnv
  ident :: Identity <- peek
  -- dbQuery1 $(selectQuery (selectAssetSegment 'ident 'seg) "$WHERE slot_asset.asset = ${ai} AND slot_asset.segment && ${seg}")
  mRow <- mapRunPrepQuery1
      ((\ _p_acO5G _p_acO5H _p_acO5I _p_acO5J _p_acO5K _p_acO5L _p_acO5M ->
                       (Data.String.fromString
                          "SELECT slot_asset.segment,asset_segment.segment,excerpt.segment,excerpt.release,asset.id,asset.format,asset.release,asset.duration,asset.name,asset.sha1,asset.size,container.id,container.top,container.name,container.date,slot_release.release,volume.id,volume.name,volume.body,volume.alias,volume.doi,volume_creation(volume.id),volume_owners.owners,volume_permission.permission,volume_permission.share_full FROM slot_asset CROSS JOIN LATERAL (VALUES (slot_asset.segment * $1)) AS asset_segment (segment) LEFT JOIN excerpt ON slot_asset.asset = excerpt.asset AND asset_segment.segment <@ excerpt.segment JOIN asset ON slot_asset.asset = asset.id JOIN container LEFT JOIN slot_release ON container.id = slot_release.container AND slot_release.segment = '(,)' ON slot_asset.container = container.id AND asset.volume = container.volume JOIN volume LEFT JOIN volume_owners ON volume.id = volume_owners.volume JOIN LATERAL   (VALUES      ( CASE WHEN $2              THEN enum_last(NULL::permission)              ELSE volume_access_check(volume.id, $3) END      , CASE WHEN $4              THEN null              ELSE (select share_full                    from volume_access_view                    where volume = volume.id and party = $5                    limit 1) END )   ) AS volume_permission (permission, share_full) ON volume_permission.permission >= 'PUBLIC'::permission ON asset.volume = volume.id WHERE slot_asset.asset = $6 AND slot_asset.segment && $7",
                       [pgEncodeParameter
                          _tenv_acO5F (PGTypeProxy :: PGTypeName "segment") _p_acO5G,
                        pgEncodeParameter
                          _tenv_acO5F (PGTypeProxy :: PGTypeName "boolean") _p_acO5H,
                        pgEncodeParameter
                          _tenv_acO5F (PGTypeProxy :: PGTypeName "integer") _p_acO5I,
                        pgEncodeParameter
                          _tenv_acO5F (PGTypeProxy :: PGTypeName "boolean") _p_acO5J,
                        pgEncodeParameter
                          _tenv_acO5F (PGTypeProxy :: PGTypeName "integer") _p_acO5K,
                        pgEncodeParameter
                          _tenv_acO5F (PGTypeProxy :: PGTypeName "integer") _p_acO5L,
                        pgEncodeParameter
                          _tenv_acO5F (PGTypeProxy :: PGTypeName "segment") _p_acO5M],
                       [pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "segment"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "segment"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "segment"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "release"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "integer"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "smallint"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "release"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "interval"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "bytea"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "bigint"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "integer"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "boolean"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "date"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "release"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "integer"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "text"),
                        pgBinaryColumn
                          _tenv_acO5F (PGTypeProxy :: PGTypeName "character varying"),
                        pgBinaryColumn
                          _tenv_acO5F (PGTypeProxy :: PGTypeName "character varying"),
                        pgBinaryColumn
                          _tenv_acO5F (PGTypeProxy :: PGTypeName "timestamp with time zone"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "text[]"),
                        pgBinaryColumn
                          _tenv_acO5F (PGTypeProxy :: PGTypeName "permission"),
                        pgBinaryColumn _tenv_acO5F (PGTypeProxy :: PGTypeName "boolean")]))
         seg
         (identitySuperuser ident)
         (view ident :: Id Party)
         (identitySuperuser ident)
         (view ident :: Id Party)
         ai
         seg)
               (\
                  [_csegment_acO5N,
                   _csegment_acO5O,
                   _csegment_acO5P,
                   _crelease_acO5Q,
                   _cid_acO5R,
                   _cformat_acO5S,
                   _crelease_acO5T,
                   _cduration_acO5U,
                   _cname_acO5V,
                   _csha1_acO5W,
                   _csize_acO5X,
                   _cid_acO5Y,
                   _ctop_acO5Z,
                   _cname_acO60,
                   _cdate_acO61,
                   _crelease_acO62,
                   _cid_acO63,
                   _cname_acO64,
                   _cbody_acO65,
                   _calias_acO66,
                   _cdoi_acO67,
                   _cvolume_creation_acO68,
                   _cowners_acO69,
                   _cpermission_acO6a,
                   _cshare_full_acO6b]
                  -> (pgDecodeColumnNotNull
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "segment") _csegment_acO5N, 
                      pgDecodeColumn
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "segment") _csegment_acO5O, 
                      pgDecodeColumnNotNull
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "segment") _csegment_acO5P, 
                      pgDecodeColumn
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "release") _crelease_acO5Q, 
                      pgDecodeColumnNotNull
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "integer") _cid_acO5R, 
                      pgDecodeColumnNotNull
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "smallint") _cformat_acO5S, 
                      pgDecodeColumn
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "release") _crelease_acO5T, 
                      pgDecodeColumn
                        _tenv_acO5F
                        (PGTypeProxy :: PGTypeName "interval")
                        _cduration_acO5U, 
                      pgDecodeColumn
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "text") _cname_acO5V, 
                      pgDecodeColumn
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "bytea") _csha1_acO5W, 
                      pgDecodeColumn
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "bigint") _csize_acO5X, 
                      pgDecodeColumnNotNull
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "integer") _cid_acO5Y, 
                      pgDecodeColumnNotNull
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "boolean") _ctop_acO5Z, 
                      pgDecodeColumn
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "text") _cname_acO60, 
                      pgDecodeColumn
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "date") _cdate_acO61, 
                      pgDecodeColumnNotNull
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "release") _crelease_acO62, 
                      pgDecodeColumnNotNull
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "integer") _cid_acO63, 
                      pgDecodeColumnNotNull
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "text") _cname_acO64, 
                      pgDecodeColumn
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "text") _cbody_acO65, 
                      pgDecodeColumn
                        _tenv_acO5F
                        (PGTypeProxy :: PGTypeName "character varying")
                        _calias_acO66, 
                      pgDecodeColumn
                        _tenv_acO5F
                        (PGTypeProxy :: PGTypeName "character varying")
                        _cdoi_acO67, 
                      pgDecodeColumn
                        _tenv_acO5F
                        (PGTypeProxy :: PGTypeName "timestamp with time zone")
                        _cvolume_creation_acO68, 
                      pgDecodeColumnNotNull
                        _tenv_acO5F (PGTypeProxy :: PGTypeName "text[]") _cowners_acO69, 
                      pgDecodeColumn
                        _tenv_acO5F
                        (PGTypeProxy :: PGTypeName "permission")
                        _cpermission_acO6a, 
                      pgDecodeColumn
                        _tenv_acO5F
                        (PGTypeProxy :: PGTypeName "boolean")
                        _cshare_full_acO6b))
  pure
    (fmap
      (\ (vsegment_acO3q, vsegment_acO3r, vsegment_acO3s, vrelease_acO3u,
          vid_acO3v, vformat_acO3w, vrelease_acO3x, vduration_acO3y,
          vname_acO3z, vc_acO3A, vsize_acO3B, vid_acO3C, vtop_acO3D,
          vname_acO3E, vdate_acO3F, vrelease_acO3G, vid_acO3H, vname_acO3J,
          vbody_acO3K, valias_acO3L, vdoi_acO3M, vc_acO3N, vowners_acO3O,
          vpermission_acO3P, vfull_acO3Q)
         -> ($)
              (makeVolumeAssetSegment
                 (makeAssetSegment
                    vsegment_acO3q
                    vsegment_acO3r
                    (do { cm_acO3T <- vsegment_acO3s;
                          Just
                            (excerptTuple
                               cm_acO3T vrelease_acO3u) }))
                 (Databrary.Model.Asset.SQL.makeAssetRow
                    vid_acO3v
                    vformat_acO3w
                    vrelease_acO3x
                    vduration_acO3y
                    vname_acO3z
                    vc_acO3A
                    vsize_acO3B)
                 (Container
                    (ContainerRow vid_acO3C vtop_acO3D vname_acO3E vdate_acO3F)
                    vrelease_acO3G))
              (Databrary.Model.Volume.SQL.makeVolume
                 (Databrary.Model.Volume.SQL.setCreation
                    (VolumeRow
                       vid_acO3H vname_acO3J vbody_acO3K valias_acO3L vdoi_acO3M)
                    vc_acO3N)
                 vowners_acO3O
                 (Databrary.Model.Volume.SQL.makePermInfo
                    vpermission_acO3P vfull_acO3Q)))
      mRow)

lookupSlotAssetSegment :: (MonadHasIdentity c m, MonadDB c m) => Id Slot -> Id Asset -> m (Maybe AssetSegment)
lookupSlotAssetSegment (Id (SlotId ci seg)) ai = do
  ident :: Identity <- peek
  dbQuery1 $(selectQuery (selectAssetSegment 'ident 'seg) 
    "$WHERE slot_asset.container = ${ci} AND slot_asset.asset = ${ai} AND slot_asset.segment && ${seg}")

lookupOrigSlotAssetSegment :: (MonadHasIdentity c m, MonadDB c m) => Id Slot -> Id Asset -> m (Maybe AssetSegment)
lookupOrigSlotAssetSegment (Id (SlotId ci seg)) ai = do
  ident :: Identity <- peek
  dbQuery1 $(selectQuery (selectAssetSegment 'ident 'seg) 
    "$inner join asset_revision ar on ar.asset = asset.id WHERE slot_asset.container = ${ci} AND slot_asset.asset = ${ai} AND slot_asset.segment && ${seg}")


lookupAssetSlotSegment :: MonadDB c m => AssetSlot -> Segment -> m (Maybe AssetSegment)
lookupAssetSlotSegment a s =
  (segmentEmpty seg) `unlessReturn` (as <$>
    dbQuery1 $(selectQuery excerptRow "$WHERE asset = ${view a :: Id Asset} AND segment @> ${seg}"))
  where
  as = makeExcerpt a s
  seg = assetSegment $ as Nothing

lookupSlotSegmentThumb :: MonadDB c m => Slot -> m (Maybe AssetSegment)
lookupSlotSegmentThumb (Slot c s) = do
  dbQuery1 $ assetSegmentInterp 0.25 . ($ c) <$> $(selectQuery (selectContainerAssetSegment 's) "$\
    \JOIN format ON asset.format = format.id \
    \WHERE slot_asset.container = ${containerId $ containerRow c} AND slot_asset.segment && ${s} \
      \AND COALESCE(asset.release, ${containerRelease c}) >= ${readRelease (view c)}::release \
      \AND (asset.duration IS NOT NULL AND format.mimetype LIKE 'video/%' OR format.mimetype LIKE 'image/%') \
    \LIMIT 1")

mapQuery :: ByteString -> ([PGValue] -> a) -> PGSimpleQuery a
mapQuery qry mkResult =
  fmap mkResult (rawPGSimpleQuery qry)

auditAssetSegmentDownload :: MonadAudit c m => Bool -> AssetSegment -> m ()
auditAssetSegmentDownload success AssetSegment{ segmentAsset = AssetSlot{ slotAsset = a, assetSlot = as }, assetSegment = seg } = do  
  ai <- getAuditIdentity
  let _tenv_a9v9T = unknownPGTypeEnv
  maybe
    (dbExecute1'
       {- [pgSQL|INSERT INTO audit.asset (audit_action, audit_user, audit_ip, id, volume, format, release) VALUES
           (${act}, ${auditWho ai}, ${auditIp ai}, ${assetId $ assetRow a}, ${volumeId $ volumeRow $ assetVolume a}, ${formatId $ assetFormat $ assetRow a}, ${assetRelease $ assetRow a})|] -}
      (mapQuery
          ((\ _p_a9v9U _p_a9v9V _p_a9v9W _p_a9v9X _p_a9v9Y _p_a9v9Z _p_a9va0 ->
                    (Data.ByteString.concat
                       [Data.String.fromString
                          "INSERT INTO audit.asset (audit_action, audit_user, audit_ip, id, volume, format, release) VALUES\n\
                          \      (",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9v9T
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "audit.action")
                          _p_a9v9U,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9v9T
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a9v9V,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9v9T
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          _p_a9v9W,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9v9T
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a9v9X,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9v9T
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a9v9Y,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9v9T
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                          _p_a9v9Z,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9v9T
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "release")
                          _p_a9va0,
                        Data.String.fromString ")"]))
      act
      (auditWho ai)
      (auditIp ai)
      (assetId $ assetRow a)
      (volumeId $ volumeRow $ assetVolume a)
      (formatId $ assetFormat $ assetRow a)
      (assetRelease $ assetRow a))
            (\[] -> ())))
    (\s -> dbExecute1' [pgSQL|$INSERT INTO audit.slot_asset (audit_action, audit_user, audit_ip, container, segment, asset) VALUES
      (${act}, ${auditWho ai}, ${auditIp ai}, ${containerId $ containerRow $ slotContainer s}, ${seg}, ${assetId $ assetRow a})|])
    as
  where act | success = AuditActionOpen
            | otherwise = AuditActionAttempt

assetSegmentJSON :: JSON.ToObject o => AssetSegment -> o
assetSegmentJSON as@AssetSegment{..} =
     "segment" JSON..= assetSegment
  <> "format" `JSON.kvObjectOrEmpty` (if view segmentAsset == fmt then empty else pure (formatId fmt))
  -- "release" `JSON.kvObjectOrEmpty` (view as :: Maybe Release)
  <> "permission" JSON..= dataPermission3 getAssetSegmentRelease2 getAssetSegmentVolumePermission2 as
  <> "excerpt" `JSON.kvObjectOrEmpty` (excerptRelease <$> assetExcerpt)
  where fmt = view as

assetSegmentInterp :: Float -> AssetSegment -> AssetSegment
assetSegmentInterp f as = as{ assetSegment = segmentInterp f $ assetSegment as }
