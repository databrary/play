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
import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Types
import qualified Database.PostgreSQL.Typed.Query
import qualified Database.PostgreSQL.Typed.Types
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

lookupAssetSegment :: (MonadHasIdentity c m, MonadDB c m) => Segment -> Id Asset -> m (Maybe AssetSegment)
lookupAssetSegment seg ai = do
  ident :: Identity <- peek
  dbQuery1 $(selectQuery (selectAssetSegment 'ident 'seg) "$WHERE slot_asset.asset = ${ai} AND slot_asset.segment && ${seg}")

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
  segmentEmpty seg ?!$> as <$>
    dbQuery1 $(selectQuery excerptRow "$WHERE asset = ${view a :: Id Asset} AND segment @> ${seg}")
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
  <> "format" JSON..=? (if view segmentAsset == fmt then empty else pure (formatId fmt))
  -- "release" JSON..=? (view as :: Maybe Release)
  <> "permission" JSON..= dataPermission3 getAssetSegmentRelease2 getAssetSegmentVolumePermission2 as
  <> "excerpt" JSON..=? (excerptRelease <$> assetExcerpt)
  where fmt = view as

assetSegmentInterp :: Float -> AssetSegment -> AssetSegment
assetSegmentInterp f as = as{ assetSegment = segmentInterp f $ assetSegment as }
