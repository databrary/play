{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Ingest
  ( viewIngest
  , postIngest
  , detectParticipantCSV
  , runParticipantUpload
  ) where

import Control.Arrow (right)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Attoparsec.ByteString as ATTO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as CSV
import qualified Data.Csv.Parser as CSVP
import qualified Data.Map as MAP
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Data.Word (Word64)
import Network.HTTP.Types (badRequest400)
import Network.Wai.Parse (FileInfo(..))
import System.Posix.FilePath (takeExtension)

import qualified Databrary.JSON as JSON
import Databrary.Ops
import Databrary.Has
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Record
import Databrary.Model.Ingest (detectBestHeaderMapping, headerMappingJSON, HeaderMappingEntry(..))
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
import Databrary.View.Ingest
import Databrary.View.Form (FormHtml)

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

maxCsvSize :: Word64
maxCsvSize = 16*1024*1024

-- TODO: maybe put csv file save/retrieve in Databrary.Store module
detectParticipantCSV :: ActionRoute (Id Volume)
detectParticipantCSV = action POST (pathJSON >/> pathId </< "detectParticipantCSV") $ \vi -> withAuth $ do
    -- checkMemberADMIN to start
    v <- getVolume PermissionEDIT vi
    reqCtxt <- peek
    csvFileInfo <-
      -- TODO: is Nothing okay here?
      runFormFiles [("file", maxCsvSize)] (Nothing :: Maybe (RequestContext -> FormHtml TL.Text)) $ do
          csrfForm
          fileInfo :: (FileInfo TL.Text) <- "file" .:> deform
          return fileInfo
    let -- uploadFileContents = "idcol\nA1\nA2\n" -- TODO: handle nothing
        uploadFileContents = (BSL.toStrict . TLE.encodeUtf8 . fileContent) csvFileInfo
        uploadFileName = (BSC.unpack . fileName) csvFileInfo  -- TODO: add prefix to filename
    let eCsvHeaders = ATTO.parseOnly (CSVP.csvWithHeader CSVP.defaultDecodeOptions) uploadFileContents
    case eCsvHeaders of
        Left err ->
            pure (forbiddenResponse reqCtxt)
        Right (hdrs, _) -> do
            let mMpng = detectBestHeaderMapping (getHeaders hdrs)
            case mMpng of
                Just mpng -> do
                    liftIO (BS.writeFile ("/tmp/" ++ uploadFileName) uploadFileContents)
                    pure
                        $ okResponse []
                            $ JSON.recordEncoding -- TODO: not record encoding
                                $ JSON.Record vi
                                    $      "csv_upload_id" JSON..= (uploadFileName)
                                        <> "suggested_mapping" JSON..= headerMappingJSON mpng
                Nothing ->
                  -- if detect headers failed, then don't save csv file and response is error
                  pure (forbiddenResponse reqCtxt) -- place holder for error

getHeaders :: CSV.Header -> [Text]
getHeaders hdrs =
  (V.toList . fmap TE.decodeUtf8) hdrs

runParticipantUpload :: ActionRoute (Id Volume)
runParticipantUpload = action POST (pathJSON >/> pathId </< "runParticipantUpload") $ \vi -> withAuth $ do
    -- checkMemberADMIN to start
    v <- getVolume PermissionEDIT vi
    (csvUploadId :: String, selectedMapping :: JSON.Value) <- runForm (Nothing) $ do
        csrfForm
        (uploadId :: String) <- "csv_upload_id" .:> deform
        mapping <- "selected_mapping" .:> deform
        pure (uploadId, mapping)
    let eMpngs = JSON.parseEither parseMapping selectedMapping
    liftIO $ print ("upload id", csvUploadId, "mapping", eMpngs)
    -- TODO: resolve csv id to absolute path; http error if unknown
    csvContents <- liftIO (readFile ("/tmp/" ++ csvUploadId)) -- TODO: cassava <<<
    pure
        $ okResponse []
            $ JSON.recordEncoding -- TODO: not record encoding
                $ JSON.Record vi $ "succeeded" JSON..= True

parseMapping :: JSON.Value -> JSON.Parser ParticipantFieldMapping -- TODO: take record description
parseMapping val = do
    (entries :: [HeaderMappingEntry]) <- JSON.parseJSON val
    let metricField :: Map Text Text
        metricField = (MAP.fromList . fmap (\e -> (hmeMetricName e, hmeCsvField e))) entries
    participantMapping <-
        case MAP.lookup "id" metricField of
            Just csvField -> pure (ParticipantFieldMapping { pfmId = Just csvField })
            Nothing -> fail "missing participant metric 'ID'"
    pure participantMapping

-- runImport :: Mapping -> 
--    bldr = mkRecordBuilder mappings
--    foreach row in csvRows
--       bldr row
--    pure result

-- mkRecordBuilder :: Map CSVField MetricName -> (CSVRow -> IO Record)
--   record <- makeRecord
--   for each (field, name)
--     metricId = getId name
--     csvVal = getCSVField row field
--     saveMeasure record metricId csvVal
--   pure record
