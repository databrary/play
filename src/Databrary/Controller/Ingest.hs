{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Ingest
  ( viewIngest
  , postIngest
  , detectParticipantCSV
  , runParticipantUpload
  -- for tests
  , mappingParser
  ) where

import Control.Arrow (right)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HMP
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Vector (Vector)
import Data.Word (Word64)
import Network.HTTP.Types (badRequest400)
import Network.Wai.Parse (FileInfo(..))
import System.Posix.FilePath (takeExtension)

import Data.Csv.Contrib (parseCsvWithHeader, getHeaders, extractColumnsDistinctSample)
import qualified Databrary.JSON as JSON
import Databrary.Ops
import Databrary.Has
import Databrary.Model.Category
import Databrary.Model.Id
import Databrary.Model.Metric (Metric)
import Databrary.Model.Ingest
import Databrary.Model.Permission
import Databrary.Model.Measure
import Databrary.Model.Metric
import Databrary.Model.Record
import Databrary.Model.Volume
import Databrary.Model.VolumeMetric
import Databrary.Model.Container
import Databrary.Ingest.Action
import Databrary.Ingest.JSON
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Form.Deform
import Databrary.Action.Route
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Volume
import Databrary.View.Form (FormHtml)
import Databrary.View.Ingest

viewIngest :: ActionRoute (Id Volume)
viewIngest = action GET (pathId </< "ingest") $ \vi -> withAuth $ do
  checkMemberADMIN
  s <- focusIO getIngestStatus
  v <- getVolume PermissionEDIT vi
  peeks $ blankForm . htmlIngestForm v s

postIngest :: ActionRoute (Id Volume)
postIngest = multipartAction $ action POST (pathId </< "ingest") $ \vi -> withAuth $ do
  checkMemberADMIN
  s <- focusIO getIngestStatus
  v <- getVolume PermissionEDIT vi
  a <- runFormFiles [("json", 16*1024*1024)] (Just $ htmlIngestForm v s) $ do
    csrfForm
    abort <- "abort" .:> deform
    abort ?!$> (,,)
      <$> ("run" .:> deform)
      <*> ("overwrite" .:> deform)
      <*> ("json" .:> do
              (fileInfo :: FileInfo JSON.Value) <- deform
              (deformCheck
                   "Must be JSON."
                   (\f -> fileContentType f `elem` ["text/json", "application/json"] || takeExtension (fileName f) == ".json")
                   fileInfo))
  r <- maybe
    (True <$ focusIO abortIngest)
    (\(r,o,j) -> runIngest $ right (map (unId . containerId . containerRow)) <$> ingestJSON v (fileContent j) r o)
    a
  unless r $ result $ response badRequest400 [] ("failed" :: String)
  peeks $ otherRouteResponse [] viewIngest (volumeId $ volumeRow v)

maxWidelyAcceptableHttpBodyFileSize :: Word64
maxWidelyAcceptableHttpBodyFileSize = 16*1024*1024

-- TODO: maybe put csv file save/retrieve in Databrary.Store module
detectParticipantCSV :: ActionRoute (Id Volume)
detectParticipantCSV = action POST (pathJSON >/> pathId </< "detectParticipantCSV") $ \vi -> withAuth $ do
    v <- getVolume PermissionEDIT vi
    reqCtxt <- peek
    csvFileInfo <-
      -- TODO: is Nothing okay here?
      runFormFiles [("file", maxWidelyAcceptableHttpBodyFileSize)] (Nothing :: Maybe (RequestContext -> FormHtml TL.Text)) $ do
          csrfForm
          fileInfo :: (FileInfo TL.Text) <- "file" .:> deform
          return fileInfo
    liftIO (print ("after extract form"))
    let uploadFileContents = (BSL.toStrict . TLE.encodeUtf8 . fileContent) csvFileInfo
    liftIO (print "uploaded contents below")
    liftIO (print uploadFileContents)
    case parseCsvWithHeader uploadFileContents of
        Left err -> do
            liftIO (print ("csv parse error", err))
            pure (forbiddenResponse reqCtxt)
        Right (hdrs, records) -> do
            participantMetrics <- lookupVolumeParticipantMetrics v
            case checkDetermineMapping participantMetrics ((fmap TE.decodeUtf8 . getHeaders) hdrs) records of
                Right columnCompatibleMetrics -> do
                    let uploadFileName = (BSC.unpack . fileName) csvFileInfo  -- TODO: add prefix to filename
                    liftIO (BS.writeFile ("/tmp/" ++ uploadFileName) uploadFileContents)
                    pure
                        $ okResponse []
                            $ JSON.recordEncoding -- TODO: not record encoding
                                $ JSON.Record vi
                                    $      "csv_upload_id" JSON..= uploadFileName
                                        <> "column_samples" JSON..= extractColumnsDistinctSample 5 hdrs records
                                        <> "suggested_mapping" JSON..= mappingToHeaderMappingEntries columnCompatibleMetrics
                                        <> "columns_firstvals" JSON..= extractColumnsDistinctSampleJson 5 hdrs records
                Left err -> do
                    liftIO (print ("failed to determine mapping", err))
                    -- if column check failed, then don't save csv file and response is error
                    pure (forbiddenResponse reqCtxt) -- place holder for error

runParticipantUpload :: ActionRoute (Id Volume)
runParticipantUpload = action POST (pathJSON >/> pathId </< "runParticipantUpload") $ \vi -> withAuth $ do
    v <- getVolume PermissionEDIT vi
    reqCtxt <- peek
    (csvUploadId :: String, selectedMapping :: JSON.Value) <- runForm (Nothing) $ do
        csrfForm
        (uploadId :: String) <- "csv_upload_id" .:> deform
        mapping <- "selected_mapping" .:> deform
        pure (uploadId, mapping)
    -- TODO: resolve csv id to absolute path; http error if unknown
    uploadFileContents <- (liftIO . BS.readFile) ("/tmp/" ++ csvUploadId)
    case parseCsvWithHeader uploadFileContents of
        Left err ->
            pure (forbiddenResponse reqCtxt) -- TODO: better error
        Right (hdrs, records) -> do
            participantActiveMetrics <- lookupVolumeParticipantMetrics v
            let (Right mpngVal) = JSON.parseEither mappingParser selectedMapping
            let eMpngs = parseParticipantFieldMapping participantActiveMetrics (getHeaders hdrs) mpngVal
            liftIO $ print ("upload id", csvUploadId, "mapping", eMpngs)
            -- TODO: validate mappings against allowed/detected data types
            let Right mpngs = eMpngs -- TODO: handle either above
            eRes <- (runImport participantActiveMetrics v records) mpngs
            pure
                $ okResponse []
                    $ JSON.recordEncoding -- TODO: not record encoding
                        $ JSON.Record vi $ "succeeded" JSON..= True

mappingParser :: JSON.Value -> JSON.Parser (Map Metric Text)
mappingParser val = do
    (entries :: [HeaderMappingEntry]) <- JSON.parseJSON val
    pure ((Map.fromList . fmap (\e -> (hmeMetric e, hmeCsvField e))) entries)

 -- TODO: error or count
runImport :: [Metric] -> Volume -> Vector Csv.NamedRecord -> ParticipantFieldMapping -> ActionM (Vector ())
runImport activeMetrics vol records mapping =
    mapM
        (\record -> createOrUpdateRecord activeMetrics vol mapping record)
        records

data ParticipantStatus = Created Record | Found Record
    -- deriving (Show, Eq)

data MeasureUpdateAction = Upsert BS.ByteString | Unchanged
    deriving (Show, Eq)

-- validated records instead of namedrecord? use Participant type?
createOrUpdateRecord :: [Metric] -> Volume -> ParticipantFieldMapping -> Csv.NamedRecord -> ActionM () -- TODO: error or record
createOrUpdateRecord participantActiveMetrics vol mapping csvRecord = do
    let participantCategory = getCategory' (Id 1) -- TODO: use global variable
        (idVal, idMetric) = maybe (error "id missing") id (getFieldVal pfmId "id")
    mOldParticipant <- lookupVolumeParticipant vol idVal
    recordStatus <-
        case mOldParticipant of
            Nothing ->
                Created <$> addRecord (blankRecord participantCategory vol)
            Just oldParticipant ->
                pure (Found oldParticipant)
    let mInfo = getFieldVal pfmInfo "info"
        mDescription = getFieldVal pfmDescription "description"
        mBirthdate = getFieldVal pfmBirthdate "birthdate"
        mGender = getFieldVal pfmGender "gender"
        mEthnicity = getFieldVal pfmEthnicity "ethnicity"
        mGestationalAge = getFieldVal pfmGestationalAge "gestationalage"
        mPregnancyTerm = getFieldVal pfmPregnancyTerm "pregnancyterm"
        mBirthWeight = getFieldVal pfmBirthWeight "birthweight"
        mDisability = getFieldVal pfmDisability "disability"
        mLanguage = getFieldVal pfmLanguage "language"
        mCountry = getFieldVal pfmCountry "country"
        mState = getFieldVal pfmState "state"
        mSetting = getFieldVal pfmSetting "setting"
    -- print ("save measure id:", mId)
    changeRecordMeasureIfUsed recordStatus (Just (idVal, idMetric))
    changeRecordMeasureIfUsed recordStatus mInfo
    changeRecordMeasureIfUsed recordStatus mDescription
    changeRecordMeasureIfUsed recordStatus mBirthdate
    changeRecordMeasureIfUsed recordStatus mGender
    changeRecordMeasureIfUsed recordStatus mEthnicity
    changeRecordMeasureIfUsed recordStatus mGestationalAge
    changeRecordMeasureIfUsed recordStatus mPregnancyTerm
    changeRecordMeasureIfUsed recordStatus mBirthWeight
    changeRecordMeasureIfUsed recordStatus mDisability
    changeRecordMeasureIfUsed recordStatus mLanguage
    changeRecordMeasureIfUsed recordStatus mCountry
    changeRecordMeasureIfUsed recordStatus mState
    changeRecordMeasureIfUsed recordStatus mSetting
  where
    getFieldVal :: (ParticipantFieldMapping -> Maybe Text) -> Text -> Maybe (BS.ByteString, Metric)
    getFieldVal extractColumnName metricSymbolicName =
        case extractColumnName mapping of
            Just columnName ->
                case HMP.lookup (TE.encodeUtf8 columnName) csvRecord of
                    Just fieldVal ->
                        pure (fieldVal, findMetricBySymbolicName metricSymbolicName) -- <<<<<<<< lookup metric
                    Nothing -> do
                        -- print ("couldn't find col", idCol)
                        Nothing -- TODO: error ... impossible?
            Nothing ->
                Nothing -- field isn't used by this volume, so don't need to save the measure
    findMetricBySymbolicName :: Text -> Metric  -- TODO: copied from above, move to shared function
    findMetricBySymbolicName symbolicName =
        (fromJust . L.find (\m -> (T.filter (/= ' ') . T.toLower . metricName) m == symbolicName)) participantActiveMetrics
    changeRecordMeasureIfUsed :: ParticipantStatus -> Maybe (BS.ByteString, Metric) -> ActionM ()
    changeRecordMeasureIfUsed recordStatus mValueMetric =
        case mValueMetric of
            Just (val, met) -> do
                case recordStatus of
                    Created record ->
                        void (changeRecordMeasure (Measure record met val))
                    Found record -> do
                        -- TODO: 
                        -- mOldVal <- getOldVal metric record
                        -- action = maybe (Upsert val) (\o -> if o == val then Unchanged else Upsert val)
                        let measureAction = Upsert val
                        case measureAction of
                            Upsert newVal ->
                                void (changeRecordMeasure (Measure record met newVal))
                            Unchanged ->
                                pure ()
            Nothing ->
                pure ()
