{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, DataKinds #-}
module Databrary.Model.Record
  ( module Databrary.Model.Record.Types
  , lookupRecord
  , lookupVolumeRecord
  , lookupVolumeRecords
  , addRecord
  , changeRecord
  , removeRecord
  , recordJSON
--  , recordJSONRestricted
  -- for testing only
  , extractParticipantFieldRows
  ) where

import Control.Monad (guard)
import qualified Data.ByteString as BS
import qualified Data.Csv as CSV
import Data.Either (isRight)
import Data.Monoid ((<>))
import qualified Data.Vector as V
import Data.Vector (Vector)

import Data.Csv.Contrib (extractColumnDefaulting)
import Databrary.Has (peek, view)
import Databrary.Service.DB
import qualified Databrary.JSON as JSON
import Databrary.Model.SQL
import Databrary.Model.Audit
import Databrary.Model.Id
import Databrary.Model.Identity.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Party.Types
import Databrary.Model.Category
import Databrary.Model.Measure
import Databrary.Model.Record.Types
import Databrary.Model.Record.SQL

lookupRecord :: (MonadHasIdentity c m, MonadDB c m) => Id Record -> m (Maybe Record)
lookupRecord ri = do
  ident <- peek
  dbQuery1 $(selectQuery (selectRecord 'ident) "$WHERE record.id = ${ri}")

lookupVolumeRecord :: MonadDB c m => Volume -> Id Record -> m (Maybe Record)
lookupVolumeRecord vol ri =
  dbQuery1 $ fmap ($ vol) $(selectQuery selectVolumeRecord "$WHERE record.id = ${ri} AND record.volume = ${volumeId $ volumeRow vol}")

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

{-
extractParticipantFieldRowsJson :: Int -> [BS.ByteString] -> Vector CSV.NamedRecord -> [JSON.Value]
extractParticipantFieldRowsJson maxRows participantFieldHeaders records =
    ( fmap (\(colHdr, vals) -> columnSampleJson colHdr vals)
    . extractParticipantFieldRows participantFieldHeaders)
    (V.take maxRows records)
-}

extractParticipantFieldRows :: [BS.ByteString] -> Vector CSV.NamedRecord -> [(BS.ByteString, [BS.ByteString])]
extractParticipantFieldRows participantFieldHeaders records =
    (zip participantFieldHeaders . fmap (\hdr -> extractColumnDefaulting hdr records)) participantFieldHeaders

-- columnSampleJson = undefined
{-
extractColumnsFirstVals :: Int -> CSV.Header -> V.Vector CSV.NamedRecord -> [JSON.Value] -- TODO: duplicated
extractColumnsFirstVals maxRows hdrs records =
    (V.toList . fmap (\hdr -> sampleColumnJson False maxRows hdr (extractColumn hdr))) hdrs
  where
    extractColumn :: BS.ByteString -> [Maybe BS.ByteString]  -- Should error out if receive nothing
    extractColumn hdr = 
        V.toList (fmap (\rowMap -> HMP.lookup hdr rowMap) records)

extractSampleColumns :: Int -> CSV.Header -> V.Vector CSV.NamedRecord -> [JSON.Value]
extractSampleColumns maxSamples hdrs records =
    (V.toList . fmap (\hdr -> sampleColumnJson True maxSamples hdr (extractColumn hdr))) hdrs
  where
    extractColumn :: BS.ByteString -> [Maybe BS.ByteString]  -- Should error out if receive nothing
    extractColumn hdr = 
        V.toList (fmap (\rowMap -> HMP.lookup hdr rowMap) records)

sampleColumnJson :: Bool -> Int -> BS.ByteString -> [Maybe BS.ByteString] -> JSON.Value
sampleColumnJson useDistinct maxSamples hdr columnValues =
    let
        uniqueSamples = (take maxSamples . (if useDistinct then L.nub else fmap id) . fmap (maybe "" id)) columnValues
    in
        JSON.object [
            "column_name" JSON..= hdr
          , "samples" JSON..= uniqueSamples
          ]
-}
