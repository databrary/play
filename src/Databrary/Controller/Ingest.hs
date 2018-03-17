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
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Word (Word64)
import Network.HTTP.Types (badRequest400)
import Network.Wai.Parse (FileInfo(..))
import System.Posix.FilePath (takeExtension)

import Data.Csv.Contrib (parseCsvWithHeader, getHeaders, extractColumnsDistinctSample)
import qualified Databrary.JSON as JSON
import Databrary.Ops
import Databrary.Has
import Databrary.Model.Id
import Databrary.Model.Metric (Metric)
import Databrary.Model.Ingest
import Databrary.Model.Permission
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
    pure $ okResponse [] ("" :: String)

mappingParser :: JSON.Value -> JSON.Parser (Map Metric Text)
mappingParser val = do
    (entries :: [HeaderMappingEntry]) <- JSON.parseJSON val
    pure ((Map.fromList . fmap (\e -> (hmeMetric e, hmeCsvField e))) entries)
