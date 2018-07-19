{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, DataKinds #-}
module Model.Record
  ( module Model.Record.Types
  , lookupRecord
  , lookupVolumeRecord
  , lookupVolumeParticipant
  , lookupVolumeRecords
  , addRecord
  , changeRecord
  , removeRecord
  , recordJSON
  , columnSampleJson
--  , recordJSONRestricted
  -- for testing only
  , extractParticipantFieldRows
  ) where

import Control.Monad (guard)
import qualified Data.ByteString as BS
import qualified Data.Csv as CSV
import Data.Either (isRight)
import Data.List (find)
import Data.Monoid ((<>))
import Data.Vector (Vector)

import Data.Csv.Contrib (extractColumnDefaulting)
import Has (peek, view)
import Service.DB
import qualified JSON as JSON
import Model.SQL
import Model.Audit
import Model.Id
import Model.Identity.Types
import Model.Volume.Types
import Model.Party.Types
import Model.Category
import Model.Measure
import Model.Metric
import Model.Record.Types
import Model.Record.SQL

lookupRecord :: (MonadHasIdentity c m, MonadDB c m) => Id Record -> m (Maybe Record)
lookupRecord ri = do
  ident <- peek
  dbQuery1 $(selectQuery (selectRecord 'ident) "$WHERE record.id = ${ri}")

lookupVolumeRecord :: MonadDB c m => Volume -> Id Record -> m (Maybe Record)
lookupVolumeRecord vol ri =
  dbQuery1 $ fmap ($ vol) $(selectQuery selectVolumeRecord "$WHERE record.id = ${ri} AND record.volume = ${volumeId $ volumeRow vol}")

lookupVolumeParticipant :: MonadDB c m => Volume -> MeasureDatum -> m (Maybe Record)
lookupVolumeParticipant vol idMeasureVal = do
    allVolumeRecords <- lookupVolumeRecords vol
    let mIdMatch = find matchesId allVolumeRecords
    -- check record type is participant, if not, then error
    pure mIdMatch
  where
    idMetric :: Metric
    idMetric = getMetric' (Id 1)
    matchesId :: Record -> Bool
    matchesId r =
         maybe
           False
           (const True)
           (find (\msr -> measureMetric msr == idMetric && measureDatum msr == idMeasureVal) (recordMeasures r))

lookupVolumeRecords :: MonadDB c m => Volume -> m [Record]
lookupVolumeRecords vol =
  dbQuery $ fmap ($ vol) $(selectQuery selectVolumeRecord "$WHERE record.volume = ${volumeId $ volumeRow vol}")

addRecord :: MonadAudit c m => Record -> m Record
addRecord br = do
  ident <- getAuditIdentity
  dbQuery1' $(insertRecord 'ident 'br)

changeRecord :: MonadAudit c m => Record -> m ()
changeRecord r = do
  ident <- getAuditIdentity
  dbExecute1' $(updateRecord 'ident 'r)

removeRecord :: MonadAudit c m => Record -> m Bool
removeRecord r = do
  ident <- getAuditIdentity
  isRight <$> dbTryJust (guard . isForeignKeyViolation) (dbExecute1 $(deleteRecord 'ident 'r))

recordJSON :: JSON.ToNestedObject o u => Bool -> Record -> JSON.Record (Id Record) o
recordJSON publicRestricted r@Record{ recordRow = RecordRow{..}, ..} = JSON.Record recordId $
  -- "volume" JSON..= volumeId recordVolume
     "category" JSON..= categoryId recordCategory
  <> "measures" JSON..=. measuresJSON publicRestricted (getRecordMeasures r)

extractParticipantFieldRows :: [BS.ByteString] -> Vector CSV.NamedRecord -> [(BS.ByteString, [BS.ByteString])]
extractParticipantFieldRows participantFieldHeaders records =
    (zip participantFieldHeaders . fmap ((`extractColumnDefaulting` records))) participantFieldHeaders

columnSampleJson :: BS.ByteString -> [BS.ByteString] -> JSON.Value
columnSampleJson hdr sampleValues =
    JSON.object [
          "column_name" JSON..= hdr
        , "samples" JSON..= sampleValues
        ]
