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
  , checkDetermineMapping
  , headerMappingJSON
  , HeaderMappingEntry(..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.Csv as CSV
import qualified Data.HashMap.Strict as HMP
import Data.List (find, (\\))
import Data.Map (Map)
import qualified Data.Map as MAP
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.PostgreSQL.Typed.Query (pgSQL)

import Databrary.Service.DB
import qualified Databrary.JSON as JSON
import Databrary.JSON (FromJSON, ToJSON)
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
import Databrary.Model.Metric.Types
import Databrary.Model.Record.Types
import Databrary.Model.Record.SQL
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL

type IngestKey = T.Text

lookupIngestContainer :: MonadDB c m => Volume -> IngestKey -> m (Maybe Container)
lookupIngestContainer vol k =
  dbQuery1 $ fmap ($ vol) $(selectQuery selectVolumeContainer "JOIN ingest.container AS ingest USING (id, volume) WHERE ingest.key = ${k} AND container.volume = ${volumeId $ volumeRow vol}")

addIngestContainer :: MonadDB c m => Container -> IngestKey -> m ()
addIngestContainer c k =
  dbExecute1' [pgSQL|INSERT INTO ingest.container (id, volume, key) VALUES (${containerId $ containerRow c}, ${volumeId $ volumeRow $ containerVolume c}, ${k})|]

lookupIngestRecord :: MonadDB c m => Volume -> IngestKey -> m (Maybe Record)
lookupIngestRecord vol k =
  dbQuery1 $ fmap ($ vol) $(selectQuery selectVolumeRecord "JOIN ingest.record AS ingest USING (id, volume) WHERE ingest.key = ${k} AND record.volume = ${volumeId $ volumeRow vol}")

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

extractColumn :: BS.ByteString -> Vector CSV.NamedRecord -> [BS.ByteString]  -- TODO: repeated in extractSampleColumns
extractColumn hdr records =
    V.toList (fmap (\rowMap -> maybe "" id (HMP.lookup hdr rowMap)) records)

-- verify that all expected columns are present, with some leniency
-- left if not enough columns or other mismatch
-- TODO: use abstract type instead of CSV's NamedRecord?
checkDetermineMapping :: [Metric] -> [Text] -> Vector CSV.NamedRecord -> Either String (Map Text [Metric])
checkDetermineMapping participantActiveMetrics csvHeaders csvRows = do
    let pairs = (zip csvHeaders . fmap detectMatches) csvHeaders
        columnCompatibleMetrics = MAP.fromList pairs
    -- TODO: check for unsolvable matchings (column with no metrics; two columns with only 1 solution)..Its a CSP!
    pure columnCompatibleMetrics
  where
    detectMatches :: Text -> [Metric]
    detectMatches hdr =
        let column = extractColumn (TE.encodeUtf8 hdr) csvRows
        in detectMatches' hdr column participantActiveMetrics
    detectMatches' :: Text -> [BS.ByteString] -> [Metric] -> [Metric]
    detectMatches' hdr columnValues activeMetrics =
        filter (columnMetricCompatible hdr columnValues) activeMetrics

columnMetricCompatible :: Text -> [BS.ByteString] -> Metric -> Bool
columnMetricCompatible hdr columnValues metric =
    T.toLower (metricName metric) == T.toLower hdr

headerMappingJSON :: Map Text [Metric] -> [JSON.Value] -- TODO: Value or list of Value?
headerMappingJSON columnCompatibleMetrics =
    ( (fmap
          (\(colName, metrics) ->
               JSON.object [ "csv_field" JSON..= colName, "compatible_metrics" JSON..= (fmap (T.toLower . metricName) metrics) ]))
    . MAP.toList )
      columnCompatibleMetrics

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
