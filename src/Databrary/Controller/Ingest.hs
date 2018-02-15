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
import Data.Monoid ((<>))
import Data.Text (Text)
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
import Databrary.Model.Ingest (detectBestHeaderMapping, headerMappingJSON)
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
      <*> ("json" .:>
        (deformCheck "Must be JSON." (\f ->
          fileContentType f `elem` ["text/json", "application/json"] || takeExtension (fileName f) == ".json")
        =<< deform))
  r <- maybe
    (True <$ focusIO abortIngest)
    (\(r,o,j) -> runIngest $ right (map (unId . containerId . containerRow)) <$> ingestJSON v (fileContent j) r o)
    a
  unless r $ result $ response badRequest400 [] ("failed" :: String)
  peeks $ otherRouteResponse [] viewIngest (volumeId $ volumeRow v)


-- TODO: maybe put csv file save/retrieve in Databrary.Store module
detectParticipantCSV :: ActionRoute (Id Volume)
detectParticipantCSV = action POST (pathJSON >/> pathId </< "detectParticipantCSV") $ \vi -> withAuth $ do
    -- checkMemberADMIN to start
    v <- getVolume PermissionEDIT vi
    let uploadFileContents = "idcol\nA1\nA2\n"
    
    let csvHeaders = ["idcol"]
        mMpng = detectBestHeaderMapping csvHeaders
    reqCtxt <- peek
    case mMpng of
        Just mpng ->
            pure
                $ okResponse []
                    $ JSON.recordEncoding -- TODO: not record encoding
                        $ JSON.Record vi
                            $      "csv_upload_id" JSON..= ("yo.csv" :: String)
                                <> "suggested_mapping" JSON..= headerMappingJSON mpng
        Nothing ->
          -- if detect headers failed, then don't save csv file and response is error
          pure (forbiddenResponse reqCtxt) -- place holder for error

runParticipantUpload :: ActionRoute (Id Volume)
runParticipantUpload = action POST (pathJSON >/> pathId </< "runParticipantUpload") $ \vi -> withAuth $ do
    -- checkMemberADMIN to start
    v <- getVolume PermissionEDIT vi
    let csvUploadId = "yo.csv"
    -- parse mappings
    csvContents <- liftIO (readFile ("/home/kanishka/tmp/" ++ csvUploadId)) -- cassava
    pure
        $ okResponse []
            $ JSON.recordEncoding -- TODO: not record encoding
                $ JSON.Record vi $ "succeeded" JSON..= True

-- parseMapping :: RecordDesc -> Value -> Parser [Mapping]
--   build up list of Map of entries
--   using record description + entries to build mapping record

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
