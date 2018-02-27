{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell, QuasiQuotes, DataKinds #-}
module Databrary.Model.Asset
  ( module Databrary.Model.Asset.Types
  -- , blankAsset
  , assetBacked
  , lookupAsset
  , lookupOrigAsset
  , lookupVolumeAsset
  , addAsset
  , changeAsset
  , assetCreation
  , assetRowJSON
  , assetJSON
  -- , assetJSONRestricted
  ) where

import Control.Arrow (first)
import Data.Maybe (isNothing, isJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
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
import Databrary.Has (view, peek)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Files
import Databrary.Store.Types
import Databrary.Store.Asset
import Databrary.Model.SQL
import Databrary.Model.Time
import Databrary.Model.Audit
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Party
import Databrary.Model.Volume
import Databrary.Model.Format
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL

mapQuery :: ByteString -> ([PGValue] -> a) -> PGSimpleQuery a
mapQuery qry mkResult =
  fmap mkResult (rawPGSimpleQuery qry)

assetBacked :: Asset -> Bool
assetBacked = isJust . assetSHA1 . assetRow

lookupAsset :: (MonadHasIdentity c m, MonadDB c m) => Id Asset -> m (Maybe Asset)
lookupAsset ai = do
  ident <- peek
  dbQuery1 $(selectQuery (selectAsset 'ident) "$WHERE asset.id = ${ai}")

lookupOrigAsset :: (MonadHasIdentity c m, MonadDB c m) => Id Asset -> m (Maybe Asset)
lookupOrigAsset ai = do
  ident <- peek
  dbQuery1 $(selectQuery (selectAsset 'ident) "$left join transcode tc on tc.orig = asset.id WHERE asset.id = ${ai}")

lookupVolumeAsset :: (MonadDB c m) => Volume -> Id Asset -> m (Maybe Asset)
lookupVolumeAsset vol ai = do
  let _tenv_a87rh = unknownPGTypeEnv
  dbQuery1 $ (`Asset` vol) <$> -- $(selectQuery selectAssetRow "WHERE asset.id = ${ai} AND asset.volume = ${volumeId $ volumeRow vol}")
    fmap
      (\ (vid_a87qZ, vformat_a87r0, vrelease_a87r1, vduration_a87r2,
          vname_a87r3, vc_a87r4, vsize_a87r5)
         -> makeAssetRow
              vid_a87qZ
              vformat_a87r0
              vrelease_a87r1
              vduration_a87r2
              vname_a87r3
              vc_a87r4
              vsize_a87r5)
      (mapQuery
        ((\ _p_a87ri _p_a87rj ->
                       (Data.ByteString.concat
                          [Data.String.fromString
                             "SELECT asset.id,asset.format,asset.release,asset.duration,asset.name,asset.sha1,asset.size FROM asset WHERE asset.id = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a87rh
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a87ri,
                           Data.String.fromString " AND asset.volume = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a87rh
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a87rj]))
         ai (volumeId $ volumeRow vol))
                (\
                  [_cid_a87rk,
                   _cformat_a87rl,
                   _crelease_a87rm,
                   _cduration_a87rn,
                   _cname_a87ro,
                   _csha1_a87rp,
                   _csize_a87rq]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a87rh
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a87rk, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a87rh
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                        _cformat_a87rl, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a87rh
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "release")
                        _crelease_a87rm, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a87rh
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "interval")
                        _cduration_a87rn, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a87rh
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a87ro, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a87rh
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bytea")
                        _csha1_a87rp, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a87rh
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bigint")
                        _csize_a87rq)))

addAsset :: (MonadAudit c m, MonadStorage c m) => Asset -> Maybe RawFilePath -> m Asset
addAsset ba fp = do
  ident <- getAuditIdentity
  ba' <- maybe (return ba) (storeAssetFile ba) fp
  dbQuery1' $(insertAsset 'ident 'ba')

changeAsset :: (MonadAudit c m, MonadStorage c m) => Asset -> Maybe RawFilePath -> m Asset
changeAsset a fp = do
  ident <- getAuditIdentity
  a2 <- maybe (return a) (storeAssetFile a) fp
  dbExecute1' $(updateAsset 'ident 'a2)
  return a2

assetCreation :: MonadDB c m => Asset -> m (Maybe Timestamp, Maybe T.Text)
assetCreation a = maybe (Nothing, Nothing) (first Just) <$>
  dbQuery1 [pgSQL|$SELECT audit_time, name FROM audit.asset WHERE id = ${assetId $ assetRow a} AND audit_action = 'add' ORDER BY audit_time DESC LIMIT 1|]

assetRowJSON :: JSON.ToObject o => AssetRow -> JSON.Record (Id Asset) o
assetRowJSON AssetRow{..} = JSON.Record assetId $
     "format" JSON..= formatId assetFormat
  <> "classification" JSON..=? assetRelease
  <> "duration" JSON..=? assetDuration
  <> "pending" JSON..=? (isNothing assetSize <? isNothing assetSHA1)

assetJSON :: JSON.ToObject o => Bool -> Asset -> JSON.Record (Id Asset) o
assetJSON _ Asset{..} = assetRowJSON assetRow -- first parameter is publicRestricted

-- assetJSONRestricted :: JSON.ToObject o => Asset -> JSON.Record (Id Asset) o
-- assetJSONRestricted Asset{..} = assetRowJSON assetRow
