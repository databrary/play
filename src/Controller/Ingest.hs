{-# LANGUAGE OverloadedStrings #-}
module Controller.Ingest
  ( viewIngest
  , postIngest
  , detectParticipantCSV
  , runParticipantUpload
  -- for tests
  , mappingParser
  , buildParticipantRecordAction
  , ParticipantStatus(..)
  , MeasureUpdateAction(..)
  , ParticipantRecordAction(..)
  ) where

import Control.Arrow (right)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Vector (Vector)
import Data.Word (Word64)
import Network.HTTP.Types (badRequest400)
import Network.Wai.Parse (FileInfo(..))
import System.Posix.FilePath (takeExtension)

import Data.Csv.Contrib (parseCsvWithHeader, getHeaders, removeBomPrefixText)
import qualified JSON as JSON
import Ops
import Has
import Model.Category
import Model.Id
import Model.Metric (Metric)
import Model.Ingest
import Model.Permission
import Model.Measure
import Model.Metric
import Model.Party
import Model.Record
import Model.Volume
import Model.VolumeMetric
import Model.Container
import Ingest.Action
import Ingest.JSON
import HTTP.Path.Parser
import HTTP.Form.Deform
import Action.Route
import Action
import Controller.Paths
import Controller.Permission
import Controller.Form
import Controller.Volume
import Store.Types
import View.Form (FormHtml)
import View.Ingest

viewIngest :: ActionRoute (Id Volume)
viewIngest = action GET (pathId </< "ingest") $ \vi -> withAuth $ do
  checkMemberADMIN
  s <- focusIO getIngestStatus
  v <- getVolume PermissionEDIT vi
  peeks $ blankForm . htmlIngestForm v s

data ControlIngestRequest =
      AbortIngest Bool
    | RunIngest Bool Bool (FileInfo JSON.Value)

postIngest :: ActionRoute (Id Volume)
postIngest = multipartAction $ action POST (pathId </< "ingest") $ \vi -> withAuth $ do
  checkMemberADMIN
  s <- focusIO getIngestStatus
  v <- getVolume PermissionEDIT vi
  a <- runFormFiles [("json", 16*1024*1024)] (Just $ htmlIngestForm v s) $ do
    csrfForm
    AbortIngest abort <- AbortIngest <$> ("abort" .:> deform)
    abort `unlessReturn` (RunIngest
      <$> ("run" .:> deform)
      <*> ("overwrite" .:> deform)
      <*> ("json" .:> do
              (fileInfo :: FileInfo JSON.Value) <- deform
              (deformCheck
                   "Must be JSON."
                   (\f -> fileContentType f `elem` ["text/json", "application/json"] || takeExtension (fileName f) == ".json")
                   fileInfo)))
  r <- maybe
    (True <$ focusIO abortIngest)
    (\(RunIngest r o j) -> runIngest $ right (map (unId . containerId . containerRow)) <$> ingestJSON v (fileContent j) r o)
    a
  unless r $ result $ response badRequest400 [] ("failed" :: String)
  peeks $ otherRouteResponse [] viewIngest (volumeId $ volumeRow v)

maxWidelyAcceptableHttpBodyFileSize :: Word64
maxWidelyAcceptableHttpBodyFileSize = 16*1024*1024

data DetectParticipantCSVRequest = DetectParticipantCSVRequest (FileInfo TL.Text)

-- TODO: maybe put csv file save/retrieve in Store module
detectParticipantCSV :: ActionRoute (Id Volume)
detectParticipantCSV = action POST (pathJSON >/> pathId </< "detectParticipantCSV") $ \vi -> withAuth $ do
    v <- getVolume PermissionEDIT vi
    (auth :: SiteAuth) <- peek
    (store :: Storage) <- peek
    DetectParticipantCSVRequest csvFileInfo <-
      -- TODO: is Nothing okay here?
      runFormFiles [("file", maxWidelyAcceptableHttpBodyFileSize)] (Nothing :: Maybe (RequestContext -> FormHtml TL.Text)) $ do
          csrfForm
          fileInfo :: (FileInfo TL.Text) <- "file" .:> deform
          return (DetectParticipantCSVRequest fileInfo)
    -- liftIO (print ("after extract form"))
    let uploadFileContents' = (BSL.toStrict . TLE.encodeUtf8 . removeBomPrefixText . fileContent) csvFileInfo
    -- liftIO (print "uploaded contents below")
    -- liftIO (print uploadFileContents')
    case parseCsvWithHeader uploadFileContents' of
        Left err -> do
            -- liftIO (print ("csv parse error", err))
            pure (response badRequest400 [] err)
        Right (hdrs, records) -> do
            metrics <- lookupVolumeParticipantMetrics v
            -- liftIO (print ("before check determine", show hdrs))
            case checkDetermineMapping metrics ((fmap TE.decodeUtf8 . getHeaders) hdrs) uploadFileContents' of
                Left err -> do
                    -- liftIO (print ("failed to determine mapping", err))
                    -- if column check failed, then don't save csv file and response is error
                    pure (response badRequest400 [] err)
                Right participantFieldMapping -> do
                    let uploadFileName =
                            uniqueUploadName auth v ((BSC.unpack . fileName) csvFileInfo)
                    liftIO
                        (BS.writeFile
                             ((BSC.unpack . getStorageTempParticipantUpload uploadFileName) store)
                             uploadFileContents')
                    pure
                        $ okResponse []
                            $ JSON.recordEncoding -- TODO: not record encoding
                                $ JSON.Record vi
                                    $      "csv_upload_id" JSON..= uploadFileName
                                        -- TODO: samples for mapped columns only
                                        <> "column_samples" JSON..= extractColumnsDistinctSampleJson 5 hdrs records
                                        <> "suggested_mapping" JSON..= participantFieldMappingToJSON participantFieldMapping
                                        <> "columns_firstvals" JSON..= extractColumnsInitialJson 5 hdrs records

-- TODO: move this to Store.ParticipantUploadTemp
uniqueUploadName :: SiteAuth -> Volume -> String -> String
uniqueUploadName siteAuth vol uploadName =
    uniqueUploadName'
        ((partyId . partyRow . accountParty . siteAccount) siteAuth)
        ((volumeId . volumeRow) vol)
        uploadName

uniqueUploadName' :: Id Party -> Id Volume -> String -> String
uniqueUploadName' uid vid uploadName =
    show uid <> "-" <> show vid <> "-" <> uploadName
----- end

data RunParticipantUploadRequest = RunParticipantUploadRequest String JSON.Value

runParticipantUpload :: ActionRoute (Id Volume)
runParticipantUpload = action POST (pathJSON >/> pathId </< "runParticipantUpload") $ \vi -> withAuth $ do
    v <- getVolume PermissionEDIT vi
    (store :: Storage) <- peek
    -- reqCtxt <- peek
    RunParticipantUploadRequest csvUploadId selectedMapping <- runForm (Nothing) $ do
        csrfForm
        (uploadId :: String) <- "csv_upload_id" .:> deform
        mapping <- "selected_mapping" .:> deform
        pure (RunParticipantUploadRequest uploadId mapping)
    -- TODO: resolve csv id to absolute path; http error if unknown
    uploadFileContents <-
        (liftIO . BS.readFile) ((BSC.unpack . getStorageTempParticipantUpload csvUploadId) store)
    case JSON.parseEither mappingParser selectedMapping of
        Left err ->
            pure (response badRequest400 [] err) -- bad json shape or keys
        Right mpngVal -> do
            participantActiveMetrics <- lookupVolumeParticipantMetrics v
            case parseParticipantFieldMapping participantActiveMetrics mpngVal of
                Left err ->
                    pure (response badRequest400 [] err) -- mapping of inactive metrics or missing metric
                Right mpngs -> do
                    -- liftIO $ print ("upload id", csvUploadId, "mapping", mpngs)
                    case attemptParseRows mpngs uploadFileContents of
                        Left err ->   -- invalid value in row
                            pure (response badRequest400 [] err)
                        Right (_, records) ->
                            let response' =
                                    okResponse []
                                        $ JSON.recordEncoding -- TODO: not record encoding
                                        $       JSON.Record vi
                                        $       "succeeded"
                                        JSON..= True
                            in  response' <$ runImport v records

mappingParser :: JSON.Value -> JSON.Parser [(Metric, Text)]
mappingParser val = do
    (entries :: [HeaderMappingEntry]) <- JSON.parseJSON val
    pure (fmap (\e -> (hmeMetric e, hmeCsvField e)) entries)

-- TODO: move all below to Model.Ingest
 -- TODO: error or count
runImport :: Volume -> Vector ParticipantRecord -> Handler (Vector ())
runImport vol records =
    mapM (createOrUpdateRecord vol) records

data ParticipantStatus = Create | Found Record

data MeasureUpdateAction = Upsert Metric MeasureDatum | Delete Metric | Unchanged Metric | NoAction Metric
    deriving (Show, Eq)

data ParticipantRecordAction = ParticipantRecordAction ParticipantStatus [MeasureUpdateAction]

buildParticipantRecordAction :: ParticipantRecord -> ParticipantStatus -> ParticipantRecordAction
buildParticipantRecordAction participantRecord updatingRecord =
    let
        mId = getFieldVal' prdId participantMetricId
        mInfo = getFieldVal' prdInfo participantMetricInfo
        mDescription = getFieldVal' prdDescription participantMetricDescription
        mBirthdate = getFieldVal' prdBirthdate participantMetricBirthdate
        mGender = getFieldVal' prdGender participantMetricGender
        mRace = getFieldVal' prdRace participantMetricRace
        mEthnicity = getFieldVal' prdEthnicity participantMetricEthnicity
        mGestationalAge = getFieldVal' prdGestationalAge participantMetricGestationalAge
        mPregnancyTerm = getFieldVal' prdPregnancyTerm participantMetricPregnancyTerm
        mBirthWeight = getFieldVal' prdBirthWeight participantMetricBirthWeight
        mDisability = getFieldVal' prdDisability participantMetricDisability
        mLanguage = getFieldVal' prdLanguage participantMetricLanguage
        mCountry = getFieldVal' prdCountry participantMetricCountry
        mState = getFieldVal' prdState participantMetricState
        mSetting = getFieldVal' prdSetting participantMetricSetting
        -- print ("save measure id:", mId)
        measureActions =
            [ changeRecordMeasureIfUsed mId
            , changeRecordMeasureIfUsed mInfo
            , changeRecordMeasureIfUsed mDescription
            , changeRecordMeasureIfUsed mBirthdate
            , changeRecordMeasureIfUsed mGender
            , changeRecordMeasureIfUsed mRace
            , changeRecordMeasureIfUsed mEthnicity
            , changeRecordMeasureIfUsed mGestationalAge
            , changeRecordMeasureIfUsed mPregnancyTerm
            , changeRecordMeasureIfUsed mBirthWeight
            , changeRecordMeasureIfUsed mDisability
            , changeRecordMeasureIfUsed mLanguage
            , changeRecordMeasureIfUsed mCountry
            , changeRecordMeasureIfUsed mState
            , changeRecordMeasureIfUsed mSetting
            ]
    in
        ParticipantRecordAction updatingRecord (catMaybes measureActions)
  where
    changeRecordMeasureIfUsed :: Maybe (Maybe MeasureDatum, Metric) -> Maybe MeasureUpdateAction
    changeRecordMeasureIfUsed mValueMetric = do
        (mVal, met) <- mValueMetric
        pure (determineUpdatedMeasure mVal met)
    determineUpdatedMeasure :: Maybe MeasureDatum -> Metric -> MeasureUpdateAction
    determineUpdatedMeasure mVal met =
        case updatingRecord of
            Create ->
                maybe (NoAction met) (Upsert met) mVal
            Found _ -> do
                 -- TODO:
                 -- mOldVal <- getOldVal metric record
                 -- action = maybe (Upsert val) (\o -> if o == val then Unchanged else Upsert val)
                 let measureAction = maybe (Delete met) (Upsert met) mVal
                 measureAction
    getFieldVal' :: (ParticipantRecord -> FieldUse a) -> Metric -> Maybe (Maybe MeasureDatum, Metric)
    getFieldVal' = getFieldVal participantRecord

getFieldVal
    :: ParticipantRecord
    -> (ParticipantRecord -> FieldUse a)
    -> Metric
    -> Maybe (Maybe MeasureDatum, Metric)
getFieldVal participantRecord extractFieldVal metric =
    case extractFieldVal participantRecord of
        Field fieldVal _ -> pure (Just fieldVal, metric)
        FieldEmpty -> pure (Nothing, metric)
        FieldUnused -> Nothing
            -- field isn't used by this volume, so don't need to save the measure

createOrUpdateRecord :: Volume -> ParticipantRecord -> Handler () -- TODO: error or record
createOrUpdateRecord vol participantRecord = do
    let category = getCategory' (Id 1) -- TODO: use global variable
        mIdVal = fst $ maybe (error "id missing") id (getFieldVal' prdId participantMetricId)
        idVal = maybe (error "id empty") id mIdVal
    mOldParticipant <- lookupVolumeParticipant vol idVal
    let recordStatus =
            case mOldParticipant of
                Nothing -> Create
                Just oldParticipant -> Found oldParticipant
    -- print ("save measure id:", mId)
    case buildParticipantRecordAction participantRecord recordStatus of
        ParticipantRecordAction Create measureActs -> do
            newParticipantShell <- addRecord (blankRecord category vol) -- blankParticipantRecord
            _ <- mapM (runMeasureUpdate newParticipantShell) measureActs
            pure () -- TODO: reload participant
        ParticipantRecordAction (Found oldRecord) measureActs -> do
            _ <- mapM (runMeasureUpdate oldRecord) measureActs
            pure ()
  where
    runMeasureUpdate :: Record -> MeasureUpdateAction -> Handler (Maybe Record)
    runMeasureUpdate record act =
        case act of
            Upsert met val -> changeRecordMeasure (Measure record met val)
            Delete met -> fmap Just (removeRecordMeasure (Measure record met ""))
            Unchanged _ -> pure Nothing
            NoAction _ -> pure Nothing
    getFieldVal' :: (ParticipantRecord -> FieldUse a) -> Metric -> Maybe (Maybe MeasureDatum, Metric)
    getFieldVal' = getFieldVal participantRecord
