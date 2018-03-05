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

import qualified Data.Csv as CSV
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
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

-- verify that all expected columns are present, with some leniency
-- left if not enough columns or other mismatch
-- TODO: use abstract type instead of CSV's NamedRecord?
checkDetermineMapping :: [Metric] -> [Text] -> Vector CSV.NamedRecord -> Either String ParticipantFieldMapping
checkDetermineMapping participantMetrics csvHeaders csvRows = do
    mIdMatch <- checkDetermineMatch checkDetermineMatchId 1
    --   TODO: if there is any collision, then error
    pure ParticipantFieldMapping {
        pfmId = mIdMatch
      , pfmInfo = Nothing -- getNameIfUsed 2
      , pfmDescription = Nothing -- getNameIfUsed 3
      , pfmBirthdate = Nothing -- getNameIfUsed 4
      , pfmGender = Nothing -- getNameIfUsed 5
      , pfmRace = Nothing -- getNameIfUsed 6
      , pfmEthnicity = Nothing -- getNameIfUsed 7
      , pfmGestationalAge = Nothing -- getNameIfUsed 8
      , pfmPregnancyTerm = Nothing -- getNameIfUsed 9
      , pfmBirthWeight = Nothing -- getNameIfUsed 10
      , pfmDisability = Nothing -- getNameIfUsed 11
      , pfmLanguage = Nothing -- getNameIfUsed 12
      , pfmCountry = Nothing -- getNameIfUsed 13
      , pfmState = Nothing -- getNameIfUsed 14
      , pfmSetting = Nothing -- getNameIfUsed 15
      }
  where
    checkDetermineMatch :: ([Text] -> Vector CSV.NamedRecord -> Metric -> Maybe Text) -> Int32 -> Either String (Maybe Text)
    checkDetermineMatch checker metricId = do
        case findMetric metricId of
            Just metric ->
                maybe (Left ("checker failed for " ++ show metricId)) (Right . Just) (checker csvHeaders csvRows metric)
            Nothing ->
                Right Nothing
    findMetric :: Int32 -> Maybe Metric
    findMetric mid = find (\m -> metricId m == Id mid) participantMetrics

checkDetermineMatchId :: [Text] -> Vector CSV.NamedRecord -> Metric -> Maybe Text
checkDetermineMatchId hdrs rows _ =
    genericChecker hdrs "id"
    
genericChecker :: [Text] -> Text -> Maybe Text
genericChecker hdrs metricName =  -- TODO: case insensitive
    if metricName `elem` hdrs then Just metricName else Nothing

{-
metricsToFieldMapping :: [Metric] -> ParticipantFieldMapping
metricsToFieldMapping volParticipantMetrics =
    ParticipantFieldMapping {
        pfmId = getNameIfUsed 1
      , pfmInfo = getNameIfUsed 2
      , pfmDescription = getNameIfUsed 3
      , pfmBirthdate = getNameIfUsed 4
      , pfmGender = getNameIfUsed 5
      , pfmRace = getNameIfUsed 6
      , pfmEthnicity = getNameIfUsed 7
      , pfmGestationalAge = getNameIfUsed 8
      , pfmPregnancyTerm = getNameIfUsed 9
      , pfmBirthWeight = getNameIfUsed 10
      , pfmDisability = getNameIfUsed 11
      , pfmLanguage = getNameIfUsed 12
      , pfmCountry = getNameIfUsed 13
      , pfmState = getNameIfUsed 14
      , pfmSetting = getNameIfUsed 15
      }
-}


headerMappingJSON :: ParticipantFieldMapping -> [a] -> [JSON.Value] -- TODO: Value or list of Value?
headerMappingJSON headerMapping leftoverColumns =
    catMaybes
        [ fieldToMaybeMapping pfmId "id"
        , fieldToMaybeMapping pfmInfo "info"
        , fieldToMaybeMapping pfmDescription "description"
        , fieldToMaybeMapping pfmBirthdate "birthdate"
        , fieldToMaybeMapping pfmGender "gender"
        , fieldToMaybeMapping pfmRace "race"
        , fieldToMaybeMapping pfmEthnicity "ethnicity"
        , fieldToMaybeMapping pfmGestationalAge "gestationalAge"
        , fieldToMaybeMapping pfmPregnancyTerm "pregnancyTerm"
        , fieldToMaybeMapping pfmBirthWeight "birthWeight"
        , fieldToMaybeMapping pfmDisability "disability"
        , fieldToMaybeMapping pfmLanguage "language"
        , fieldToMaybeMapping pfmCountry "country"
        , fieldToMaybeMapping pfmState "state"
        , fieldToMaybeMapping pfmSetting "setting"
        ] -- TODO: add leftover columns
  where
    fieldToMaybeMapping :: (ParticipantFieldMapping -> Maybe Text) -> String -> Maybe JSON.Value
    fieldToMaybeMapping getField fieldMetricName = do
        colName <- getField headerMapping
        pure (JSON.object [ "csv_field" JSON..= colName, "metric" JSON..= fieldMetricName ]) -- TODO: add data_type

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
