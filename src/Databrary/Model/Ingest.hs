{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds, OverloadedStrings #-}
module Databrary.Model.Ingest
  ( IngestKey
  , lookupIngestContainer
  , addIngestContainer
  , lookupIngestRecord
  , addIngestRecord
  , lookupIngestAsset
  , addIngestAsset
  , replaceSlotAsset
  , HeaderMappingEntry(..)
  ) where

import qualified Data.Csv as CSV
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text (Text)
import Data.Map (Map)
import Database.PostgreSQL.Typed.Query (pgSQL)
import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Types
import qualified Database.PostgreSQL.Typed.Query
import qualified Database.PostgreSQL.Typed.Types
import qualified Data.ByteString
import Data.ByteString (ByteString)
import qualified Data.String
import Data.Vector (Vector)

import Data.Csv.Contrib (extractColumnsDistinctSample)
import Databrary.Service.DB
import qualified Databrary.JSON as JSON
import Databrary.JSON (FromJSON(..))
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
import Databrary.Model.Metric.Types
import qualified Databrary.Model.Record.SQL
import Databrary.Model.Record.Types
import Databrary.Model.Record (columnSampleJson)
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
addIngestRecord r k = do
  let _tenv_a6PCz = unknownPGTypeEnv
  dbExecute1' -- [pgSQL|INSERT INTO ingest.record (id, volume, key) VALUES (${recordId $ recordRow r}, ${volumeId $ volumeRow $ recordVolume r}, ${k})|]
   (mapQuery
    ((\ _p_a6PCA _p_a6PCB _p_a6PCC ->
                    (Data.ByteString.concat
                       [Data.String.fromString
                          "INSERT INTO ingest.record (id, volume, key) VALUES (",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PCz
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PCA,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PCz
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PCB,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PCz
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a6PCC,
                        Data.String.fromString ")"]))
      (recordId $ recordRow r) (volumeId $ volumeRow $ recordVolume r) k)
            (\ [] -> ()))

lookupIngestAsset :: MonadDB c m => Volume -> FilePath -> m (Maybe Asset)
lookupIngestAsset vol k = do
  let _tenv_a6PDv = unknownPGTypeEnv
  dbQuery1 $ fmap (`Asset` vol) -- $(selectQuery selectAssetRow "JOIN ingest.asset AS ingest USING (id) WHERE ingest.file = ${k} AND asset.volume = ${volumeId $ volumeRow vol}")
    (fmap
      (\ (vid_a6PDo, vformat_a6PDp, vrelease_a6PDq, vduration_a6PDr,
          vname_a6PDs, vc_a6PDt, vsize_a6PDu)
         -> Databrary.Model.Asset.SQL.makeAssetRow
              vid_a6PDo
              vformat_a6PDp
              vrelease_a6PDq
              vduration_a6PDr
              vname_a6PDs
              vc_a6PDt
              vsize_a6PDu)
     (mapQuery
      ((\ _p_a6PDw _p_a6PDx ->
                       (Data.ByteString.concat
                          [Data.String.fromString
                             "SELECT asset.id,asset.format,asset.release,asset.duration,asset.name,asset.sha1,asset.size FROM asset JOIN ingest.asset AS ingest USING (id) WHERE ingest.file = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6PDv
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "text")
                             _p_a6PDw,
                           Data.String.fromString " AND asset.volume = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6PDv
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a6PDx]))
         k (volumeId $ volumeRow vol))
               (\ [_cid_a6PDy,
                   _cformat_a6PDz,
                   _crelease_a6PDA,
                   _cduration_a6PDB,
                   _cname_a6PDC,
                   _csha1_a6PDD,
                   _csize_a6PDE]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6PDv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a6PDy, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6PDv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                        _cformat_a6PDz, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6PDv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "release")
                        _crelease_a6PDA, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6PDv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "interval")
                        _cduration_a6PDB, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6PDv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a6PDC, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6PDv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bytea")
                        _csha1_a6PDD, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a6PDv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bigint")
                        _csize_a6PDE))))

addIngestAsset :: MonadDB c m => Asset -> FilePath -> m ()
addIngestAsset r k = do
  let _tenv_a6PFc = unknownPGTypeEnv
  dbExecute1' -- [pgSQL|INSERT INTO ingest.asset (id, file) VALUES (${assetId $ assetRow r}, ${k})|]
   (mapQuery
    ((\ _p_a6PFd _p_a6PFe ->
                    (Data.ByteString.concat
                       [Data.String.fromString
                          "INSERT INTO ingest.asset (id, file) VALUES (",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PFc
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PFd,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PFc
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a6PFe,
                        Data.String.fromString ")"]))
      (assetId $ assetRow r) k)
            (\ [] -> ()))

replaceSlotAsset :: MonadDB c m => Asset -> Asset -> m Bool
replaceSlotAsset o n = do
  let _tenv_a6PFB = unknownPGTypeEnv
  dbExecute1 -- [pgSQL|UPDATE slot_asset SET asset = ${assetId $ assetRow n} WHERE asset = ${assetId $ assetRow o}|]
   (mapQuery
    ((\ _p_a6PFC _p_a6PFD ->
                    (Data.ByteString.concat
                       [Data.String.fromString "UPDATE slot_asset SET asset = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PFB
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PFC,
                        Data.String.fromString " WHERE asset = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6PFB
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6PFD]))
      (assetId $ assetRow n) (assetId $ assetRow o))
            (\ [] -> ()))

extractColumnsDistinctSampleJson :: Int -> CSV.Header -> Vector CSV.NamedRecord -> [JSON.Value]
extractColumnsDistinctSampleJson maxSamples hdrs records =
    ( fmap (\(colHdr, vals) -> columnSampleJson colHdr vals)
    . extractColumnsDistinctSample maxSamples hdrs)
    records

data HeaderMappingEntry =
    HeaderMappingEntry {
          hmeCsvField :: Text
        , hmeMetricName :: Text
    } deriving (Show, Eq, Ord)

instance FromJSON HeaderMappingEntry where
    parseJSON =
        JSON.withObject "HeaderMappingEntry"
            (\o ->
                 HeaderMappingEntry
                     <$> o JSON..: "csv_field"
                     <*> o JSON..: "metric") -- TODO: validate that it matches a real metric name

-- TODO: unit tests
-- TODO: validator should also ensure each col mentioned is a real column
parseParticipantFieldMapping :: [Metric] -> Map Text Text -> Either String ParticipantFieldMapping
parseParticipantFieldMapping volParticipantActiveMetrics requestedMapping = do
    ParticipantFieldMapping
        <$> getFieldIfUsed "id"
        <*> getFieldIfUsed "info"
        <*> getFieldIfUsed "description"
        <*> getFieldIfUsed "birthdate"
        <*> getFieldIfUsed "gender"
        <*> getFieldIfUsed "race"
        <*> getFieldIfUsed "ethnicity"
        <*> getFieldIfUsed "gestationalage" -- handled space in name
        <*> getFieldIfUsed "pregnancyterm" -- space
        <*> getFieldIfUsed "birthweight" -- space
        <*> getFieldIfUsed "disability"
        <*> getFieldIfUsed "language"
        <*> getFieldIfUsed "country"
        <*> getFieldIfUsed "state"
        <*> getFieldIfUsed "setting"
  where
    getFieldIfUsed :: Text -> Either String (Maybe Text)
    getFieldIfUsed metricSymbolicName =
        case findMetricBySymbolicName metricSymbolicName of
            Just metric ->
                case Map.lookup metricSymbolicName requestedMapping of
                    Just csvField -> pure (Just csvField)
                    Nothing -> fail "missing expected participant metric" -- TODO: name metric
            Nothing ->
                pure Nothing
    findMetricBySymbolicName :: Text -> Maybe Metric
    findMetricBySymbolicName symbolicName =
        L.find (\m -> (T.filter (/= ' ') . T.toLower . metricName) m == symbolicName) volParticipantActiveMetrics
