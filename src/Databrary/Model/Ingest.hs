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
import Data.List (find, (\\))
import Data.Map (Map)
import qualified Data.Map as MAP
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
checkDetermineMapping :: [Metric] -> [Text] -> Vector CSV.NamedRecord -> Either String (Map Text [Metric])
checkDetermineMapping participantMetrics csvHeaders csvRows = do
  Left "undefined"
  {-
    mIdMatch <- checkDetermineMatch checkDetermineMatchId 1
    mInfoMatch <- checkDetermineMatch checkDetermineMatchInfo 2
    mDescriptionMatch <- checkDetermineMatch checkDetermineMatchDescription 3
    mBirthdateMatch <- checkDetermineMatch checkDetermineMatchBirthdate 4
    mGenderMatch <- checkDetermineMatch checkDetermineMatchGender 5
    mRaceMatch <- checkDetermineMatch checkDetermineMatchRace 6
    mEthnicityMatch <- checkDetermineMatch checkDetermineMatchEthnicity 7
    mGestationalAgeMatch <- checkDetermineMatch checkDetermineMatchGestationalAge 8
    mPregnancyTermMatch <- checkDetermineMatch checkDetermineMatchPregnancyTerm 9
    mBirthWeightMatch <- checkDetermineMatch checkDetermineMatchBirthWeight 10
    mDisabilityMatch <- checkDetermineMatch checkDetermineMatchDisability 11
    mLanguageMatch <- checkDetermineMatch checkDetermineMatchLanguage 12
    mCountryMatch <- checkDetermineMatch checkDetermineMatchCountry 13
    mStateMatch <- checkDetermineMatch checkDetermineMatchState 14
    mSettingMatch <- checkDetermineMatch checkDetermineMatchSetting 15
    --   TODO: if there is any collision, then error
    let usedCols =
            catMaybes [ mIdMatch, mInfoMatch, mDescriptionMatch, mBirthdateMatch, mGenderMatch, mRaceMatch
                      , mEthnicityMatch, mGestationalAgeMatch, mPregnancyTermMatch, mBirthWeightMatch, mDisabilityMatch
                      , mLanguageMatch, mCountryMatch, mStateMatch, mSettingMatch ]
        skippedCols = csvHeaders \\ usedCols    
    pure
        (ParticipantFieldMapping {
              pfmId = mIdMatch
            , pfmInfo = mInfoMatch 
            , pfmDescription = mDescriptionMatch 
            , pfmBirthdate = mBirthdateMatch
            , pfmGender = mGenderMatch
            , pfmRace = mRaceMatch
            , pfmEthnicity = mEthnicityMatch
            , pfmGestationalAge = mGestationalAgeMatch
            , pfmPregnancyTerm = mPregnancyTermMatch
            , pfmBirthWeight = mBirthWeightMatch
            , pfmDisability = mDisabilityMatch
            , pfmLanguage = mLanguageMatch
            , pfmCountry = mCountryMatch
            , pfmState = mStateMatch
            , pfmSetting = mSettingMatch
            }
        , skippedCols
        )
  where
    checkDetermineMatch :: ([Text] -> Vector CSV.NamedRecord -> Metric -> Maybe [Text]) -> Int32 -> Either String (Maybe [Text])
    checkDetermineMatch checker metricId = do
        case findMetric metricId of
            Just metric ->
                maybe (Left ("checker failed for " ++ show metricId)) (Right . Just) (checker csvHeaders csvRows metric)
            Nothing ->
                Right Nothing
    findMetric :: Int32 -> Maybe Metric
    findMetric mid = find (\m -> metricId m == Id mid) participantMetrics
  -}

checkDetermineMatchId :: [Text] -> Vector CSV.NamedRecord -> Metric -> Maybe [Text]
checkDetermineMatchId hdrs rows _ =
    genericChecker hdrs "id"

checkDetermineMatchInfo :: [Text] -> Vector CSV.NamedRecord -> Metric -> Maybe [Text]
checkDetermineMatchInfo hdrs rows _ =
    genericChecker hdrs "info"

checkDetermineMatchDescription :: [Text] -> Vector CSV.NamedRecord -> Metric -> Maybe [Text]
checkDetermineMatchDescription hdrs rows _ =
    genericChecker hdrs "description"

checkDetermineMatchBirthdate :: [Text] -> Vector CSV.NamedRecord -> Metric -> Maybe [Text]
checkDetermineMatchBirthdate hdrs rows _ =
    genericChecker hdrs "birthdate"

checkDetermineMatchGender :: [Text] -> Vector CSV.NamedRecord -> Metric -> Maybe [Text]
checkDetermineMatchGender hdrs rows _ =
    genericChecker hdrs "gender"

checkDetermineMatchRace :: [Text] -> Vector CSV.NamedRecord -> Metric -> Maybe [Text]
checkDetermineMatchRace hdrs rows _ =
    genericChecker hdrs "race"

checkDetermineMatchEthnicity :: [Text] -> Vector CSV.NamedRecord -> Metric -> Maybe [Text]
checkDetermineMatchEthnicity hdrs rows _ =
    genericChecker hdrs "ethnicity"

checkDetermineMatchGestationalAge :: [Text] -> Vector CSV.NamedRecord -> Metric -> Maybe [Text]
checkDetermineMatchGestationalAge hdrs rows _ =
    genericChecker hdrs "gestationalage"

checkDetermineMatchPregnancyTerm :: [Text] -> Vector CSV.NamedRecord -> Metric -> Maybe [Text]
checkDetermineMatchPregnancyTerm hdrs rows _ =
    genericChecker hdrs "pregnancyterm"

checkDetermineMatchBirthWeight :: [Text] -> Vector CSV.NamedRecord -> Metric -> Maybe [Text]
checkDetermineMatchBirthWeight hdrs rows _ =
    genericChecker hdrs "birthweight"

checkDetermineMatchDisability :: [Text] -> Vector CSV.NamedRecord -> Metric -> Maybe [Text]
checkDetermineMatchDisability hdrs rows _ =
    genericChecker hdrs "disability"

checkDetermineMatchLanguage :: [Text] -> Vector CSV.NamedRecord -> Metric -> Maybe [Text]
checkDetermineMatchLanguage hdrs rows _ =
    genericChecker hdrs "language"

checkDetermineMatchCountry :: [Text] -> Vector CSV.NamedRecord -> Metric -> Maybe [Text]
checkDetermineMatchCountry hdrs rows _ =
    genericChecker hdrs "country"

checkDetermineMatchState :: [Text] -> Vector CSV.NamedRecord -> Metric -> Maybe [Text]
checkDetermineMatchState hdrs rows _ =
    genericChecker hdrs "state"

checkDetermineMatchSetting :: [Text] -> Vector CSV.NamedRecord -> Metric -> Maybe [Text]
checkDetermineMatchSetting hdrs rows _ =
    genericChecker hdrs "setting"

genericChecker :: [Text] -> Text -> Maybe [Text]
genericChecker hdrs metricName =  -- TODO: case insensitive
    if metricName `elem` hdrs then Just [metricName] else Nothing

headerMappingJSON :: Map Text [Metric] -> [JSON.Value] -- TODO: Value or list of Value?
headerMappingJSON columnCompatibleMetrics =
    ( (fmap
          (\(colName, metrics) ->
               JSON.object [ "csv_field" JSON..= colName, "compatible_metrics" JSON..= (fmap metricName metrics) ]))
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
