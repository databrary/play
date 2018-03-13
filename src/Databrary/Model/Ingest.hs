{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds #-}
module Databrary.Model.Ingest
  ( IngestKey
  , lookupIngestContainer
  , addIngestContainer
  , lookupIngestRecord
  , addIngestRecord
  , lookupIngestAsset
  , addIngestAsset
  , replaceSlotAsset
  ) where

import qualified Data.Text as T
import Database.PostgreSQL.Typed.Query (pgSQL)
import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Types
import qualified Database.PostgreSQL.Typed.Query
import qualified Database.PostgreSQL.Typed.Types
import qualified Data.ByteString
import Data.ByteString (ByteString)
import qualified Data.String

import Databrary.Service.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
import qualified Databrary.Model.Record.SQL
import Databrary.Model.Record.Types
import Databrary.Model.Record.SQL
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL

type IngestKey = T.Text

mapQuery :: ByteString -> ([PGValue] -> a) -> PGSimpleQuery a
mapQuery qry mkResult =
  fmap mkResult (rawPGSimpleQuery qry)

lookupIngestContainer :: MonadDB c m => Volume -> IngestKey -> m (Maybe Container)
lookupIngestContainer vol k = do
  let _tenv_a6Dpp = unknownPGTypeEnv
  dbQuery1 $ fmap ($ vol) -- $(selectQuery selectVolumeContainer "JOIN ingest.container AS ingest USING (id, volume) WHERE ingest.key = ${k} AND container.volume = ${volumeId $ volumeRow vol}")
    (fmap
      (\ (vid_a6Dph, vtop_a6Dpi, vname_a6Dpj, vdate_a6Dpk,
          vrelease_a6Dpl)
         -> Container
              (ContainerRow vid_a6Dph vtop_a6Dpi vname_a6Dpj vdate_a6Dpk)
              vrelease_a6Dpl)
      (mapQuery
        ((\ _p_a6Dpq _p_a6Dpr -> 
                       (Data.ByteString.concat
                          [Data.String.fromString
                             "SELECT container.id,container.top,container.name,container.date,slot_release.release FROM container LEFT JOIN slot_release ON container.id = slot_release.container AND slot_release.segment = '(,)' JOIN ingest.container AS ingest USING (id, volume) WHERE ingest.key = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6Dpp
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "text")
                             _p_a6Dpq,
                           Data.String.fromString " AND container.volume = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6Dpp
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a6Dpr]))
         k (volumeId $ volumeRow vol))
               (\ [_cid_a6Dps,
                   _ctop_a6Dpt,
                   _cname_a6Dpu,
                   _cdate_a6Dpv,
                   _crelease_a6Dpw]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6Dpp
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a6Dps, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6Dpp
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "boolean")
                        _ctop_a6Dpt, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6Dpp
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a6Dpu, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6Dpp
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "date")
                        _cdate_a6Dpv, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6Dpp
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "release")
                        _crelease_a6Dpw))))

addIngestContainer :: MonadDB c m => Container -> IngestKey -> m ()
addIngestContainer c k = do
  let _tenv_a6Dvh = unknownPGTypeEnv
  dbExecute1' -- [pgSQL|INSERT INTO ingest.container (id, volume, key) VALUES (${containerId $ containerRow c}, ${volumeId $ volumeRow $ containerVolume c}, ${k})|]
   (mapQuery
    ((\ _p_a6Dvi _p_a6Dvj _p_a6Dvk ->
                    (Data.ByteString.concat
                       [Data.String.fromString
                          "INSERT INTO ingest.container (id, volume, key) VALUES (",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6Dvh
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6Dvi,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6Dvh
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6Dvj,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6Dvh
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a6Dvk,
                        Data.String.fromString ")"]))
      (containerId $ containerRow c)
      (volumeId $ volumeRow $ containerVolume c)
      k)
            (\ [] -> ()))

lookupIngestRecord :: MonadDB c m => Volume -> IngestKey -> m (Maybe Record)
lookupIngestRecord vol k = do
  let _tenv_a6GtF = unknownPGTypeEnv
  dbQuery1 $ fmap ($ vol) -- $(selectQuery selectVolumeRecord "JOIN ingest.record AS ingest USING (id, volume) WHERE ingest.key = ${k} AND record.volume = ${volumeId $ volumeRow vol}")
    (fmap
      (\ (vid_a6GtB, vcategory_a6GtC, vmeasures_a6GtD, vc_a6GtE)
         -> ($)
              (Databrary.Model.Record.SQL.makeRecord
                 vid_a6GtB vcategory_a6GtC vmeasures_a6GtD)
              vc_a6GtE)
     (mapQuery
      ((\ _p_a6GtG _p_a6GtH ->
                       (Data.ByteString.concat
                          [Data.String.fromString
                             "SELECT record.id,record.category,record.measures,record_release(record.id) FROM record_measures AS record JOIN ingest.record AS ingest USING (id, volume) WHERE ingest.key = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6GtF
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "text")
                             _p_a6GtG,
                           Data.String.fromString " AND record.volume = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6GtF
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a6GtH]))
         k (volumeId $ volumeRow vol))
               (\ [_cid_a6GtI,
                   _ccategory_a6GtJ,
                   _cmeasures_a6GtK,
                   _crecord_release_a6GtL]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6GtF
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a6GtI, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6GtF
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                        _ccategory_a6GtJ, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6GtF
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text[]")
                        _cmeasures_a6GtK, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6GtF
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "release")
                        _crecord_release_a6GtL))))

addIngestRecord :: MonadDB c m => Record -> IngestKey -> m ()
addIngestRecord r k =
  dbExecute1' [pgSQL|INSERT INTO ingest.record (id, volume, key) VALUES (${recordId $ recordRow r}, ${volumeId $ volumeRow $ recordVolume r}, ${k})|]

lookupIngestAsset :: MonadDB c m => Volume -> FilePath -> m (Maybe Asset)
lookupIngestAsset vol k =
  dbQuery1 $ fmap (`Asset` vol) $(selectQuery selectAssetRow "JOIN ingest.asset AS ingest USING (id) WHERE ingest.file = ${k} AND asset.volume = ${volumeId $ volumeRow vol}")

addIngestAsset :: MonadDB c m => Asset -> FilePath -> m ()
addIngestAsset r k =
  dbExecute1' [pgSQL|INSERT INTO ingest.asset (id, file) VALUES (${assetId $ assetRow r}, ${k})|]

replaceSlotAsset :: MonadDB c m => Asset -> Asset -> m Bool
replaceSlotAsset o n =
  dbExecute1 [pgSQL|UPDATE slot_asset SET asset = ${assetId $ assetRow n} WHERE asset = ${assetId $ assetRow o}|]
