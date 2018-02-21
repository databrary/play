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
  , detectBestHeaderMapping
  , headerMappingJSON
  , HeaderMappingEntry(..)
  ) where

-- TODO: delete bimap from databrary.nix
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Typed.Query (pgSQL)

import Databrary.Service.DB
import qualified Databrary.JSON as JSON
import Databrary.JSON (FromJSON, ToJSON)
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
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

detectBestHeaderMapping :: [Text] -> Maybe ParticipantFieldMapping -- nothing if not enough columns or other mismatch
detectBestHeaderMapping csvHeaders =
  -- TODO read volume spreadsheet definition and use that to determine whether Just or Nothing for each field
  case csvHeaders of
      hdr1:_ ->
          Just
              (ParticipantFieldMapping
                  { pfmId = Just hdr1
                  })
      [] ->
          Nothing

headerMappingJSON :: ParticipantFieldMapping -> [JSON.Value] -- TODO: Value or list of Value?
headerMappingJSON headerMapping =
    catMaybes
        [ fmap
            (\idCol -> JSON.object [ "csv_field" JSON..= idCol, "metric" JSON..= ("id" :: String) ])
            (pfmId headerMapping)
        ]

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
