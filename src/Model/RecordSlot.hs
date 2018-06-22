{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds, ViewPatterns #-}
module Model.RecordSlot
  ( module Model.RecordSlot.Types
  , lookupRecordSlots
  , lookupSlotRecords
  , lookupContainerRecords
  , lookupRecordSlotRecords
  , lookupVolumeContainersRecords
  , lookupVolumeContainersRecordIds
  , lookupVolumeRecordSlotIds
  , moveRecordSlot
  , removeRecordAllSlot
  , recordSlotAge
  , recordSlotJSON
  ) where

import Control.Arrow (second)
import Control.Monad (guard, liftM2)
import Data.Function (on)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Database.PostgreSQL.Typed.Range as Range
import Database.PostgreSQL.Typed.Types (PGTypeName(..))

import Ops
import qualified JSON as JSON
import Service.DB
import Model.Id.Types
import Model.Segment
import Model.Permission
import Model.Audit
import Model.Audit.SQL
import Model.Volume.Types
import Model.Container.Types
import Model.Slot
import Model.Metric
import Model.Record
import Model.Age
import Model.Measure
import Model.SQL
import Model.RecordSlot.Types
import Model.RecordSlot.SQL

lookupRecordSlots :: (MonadDB c m) => Record -> m [RecordSlot]
lookupRecordSlots r =
  dbQuery $ ($ r) <$> $(selectQuery selectRecordSlotRecord "$WHERE slot_record.record = ${recordId $ recordRow r}")

lookupSlotRecords :: (MonadDB c m) => Slot -> m [RecordSlot]
lookupSlotRecords (Slot c s) =
  dbQuery $ ($ c) <$> $(selectQuery selectContainerSlotRecord "$WHERE slot_record.container = ${containerId $ containerRow c} AND slot_record.segment && ${s}")

lookupContainerRecords :: (MonadDB c m) => Container -> m [RecordSlot]
lookupContainerRecords = lookupSlotRecords . containerSlot

lookupRecordSlotRecords :: (MonadDB c m) => Record -> Slot -> m [RecordSlot]
lookupRecordSlotRecords r (Slot c s) =
  dbQuery $ ($ c) . ($ r) <$> $(selectQuery selectRecordContainerSlotRecord "WHERE slot_record.record = ${recordId $ recordRow r} AND slot_record.container = ${containerId $ containerRow c} AND slot_record.segment && ${s}")

lookupVolumeContainersRecords :: (MonadDB c m) => Volume -> m [(Container, [RecordSlot])]
lookupVolumeContainersRecords v =
  map (second catMaybes) . groupTuplesBy ((==) `on` containerId . containerRow) <$>
    dbQuery (($ v) <$> $(selectQuery selectVolumeSlotMaybeRecord "WHERE container.volume = ${volumeId $ volumeRow v} ORDER BY container.id, record.category NULLS FIRST, slot_record.segment, slot_record.record"))

lookupVolumeContainersRecordIds :: (MonadDB c m) => Volume -> m [(Container, [(Segment, Id Record)])]
lookupVolumeContainersRecordIds v =
  map (second catMaybes) . groupTuplesBy ((==) `on` containerId . containerRow) <$>
    dbQuery (($ v) <$> $(selectQuery selectVolumeSlotMaybeRecordId "$WHERE container.volume = ${volumeId $ volumeRow v} ORDER BY container.id, slot_record.segment, slot_record.record"))

lookupVolumeRecordSlotIds :: (MonadDB c m) => Volume -> m [(Record, SlotId)]
lookupVolumeRecordSlotIds v =
  dbQuery (($ v) <$> $(selectQuery selectVolumeSlotIdRecord "WHERE record.volume = ${volumeId $ volumeRow v} ORDER BY container"))

moveRecordSlot :: (MonadAudit c m) => RecordSlot -> Segment -> m Bool
moveRecordSlot rs@RecordSlot{ recordSlot = s@Slot{ slotSegment = src } } dst = do
  ident <- getAuditIdentity
  either (const False) id
    <$> case (Range.isEmpty (segmentRange src), Range.isEmpty (segmentRange dst)) of
    (True,  True) -> return $ Right False
    (False, True) -> Right <$> dbExecute1 $(deleteSlotRecord 'ident 'rs)
    (True,  False) -> dbTryJust err $ dbExecute1 $(insertSlotRecord 'ident 'rd)
    (False, False) -> dbTryJust err $ dbExecute1 $(updateSlotRecord 'ident 'rs 'dst)
  where
  rd = rs{ recordSlot = s{ slotSegment = dst } }
  err = guard . isExclusionViolation

removeRecordAllSlot :: (MonadAudit c m) => Record -> m Int
removeRecordAllSlot r = do
  ident <- getAuditIdentity
  dbExecute $(auditDelete 'ident "slot_record" "record = ${recordId $ recordRow r} AND segment = '(,)'" Nothing)

recordSlotAge :: RecordSlot -> Maybe Age
recordSlotAge rs@RecordSlot{..} =
  clip <$> liftM2 age (decodeMeasure (PGTypeProxy :: PGTypeName "date") =<< getMeasure birthdateMetric (recordMeasures slotRecord)) (containerDate $ containerRow $ slotContainer recordSlot)
  where
  clip a
    | not (canReadData2 getRecordSlotRelease getRecordSlotVolumePermission rs) = a `min` ageLimit
    | otherwise = a

recordSlotJSON :: JSON.ToObject o => Bool -> RecordSlot -> JSON.Record (Id Record) o
recordSlotJSON _ rs@RecordSlot{..} = JSON.Record (recordId $ recordRow slotRecord) $
     segmentJSON (slotSegment recordSlot)
  <> "age" `JSON.kvObjectOrEmpty` recordSlotAge rs

{-
recordSlotJSONRestricted :: JSON.ToObject o => RecordSlot -> JSON.Record (Id Record) o
recordSlotJSONRestricted rs@RecordSlot{..} = JSON.Record (recordId $ recordRow slotRecord) $
     segmentJSON (slotSegment recordSlot)
  <> "age" `JSON.kvObjectOrEmpty` recordSlotAge rs -- allow age to pass through so that summary can be computed
-}
