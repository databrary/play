{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Databrary.Model.Record.Types
  ( RecordRow(..)
  , Record(..)
  , ParticipantRecord(..)
  , FieldUse(..)
  , getRecordVolumePermission
  -- , ParticipantFieldMapping(..)
  , Measure(..)
  , Measures
  , blankRecord
  ) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.Time (Day)

import Databrary.Has (Has(..))
import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Release.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Metric.Types
import Databrary.Model.Category.Types

type instance IdType Record = Int32

data RecordRow = RecordRow
  { recordId :: Id Record
  , recordCategory :: Category
  } deriving (Show, Eq)

data Record = Record
  { recordRow :: !RecordRow
  , recordMeasures :: Measures
  , recordRelease :: Maybe Release
  , recordVolume :: Volume
  }

instance Kinded Record where
  kindOf _ = "record"

-- | States for fields within 'ParticipantRecord'.
--
-- We keep both the parsed and raw values for data for now...
data FieldUse a
    = FieldUnused -- ^ Unused/not supplied
    | FieldEmpty -- ^ Supplied, but empty
    | Field MeasureDatum a -- ^ The raw value and its converted form
    deriving (Show, Eq, Ord, Functor)

data ParticipantRecord =
    ParticipantRecord -- are some of these required?
        { prdId :: FieldUse ByteString
        , prdInfo :: FieldUse ByteString
        , prdDescription :: FieldUse ByteString
        , prdBirthdate :: FieldUse Day
        , prdGender :: FieldUse ByteString
        , prdRace :: FieldUse ByteString
        , prdEthnicity :: FieldUse ByteString
        , prdGestationalAge :: FieldUse Double
        , prdPregnancyTerm :: FieldUse ByteString
        , prdBirthWeight :: FieldUse Double
        , prdDisability :: FieldUse ByteString
        , prdLanguage :: FieldUse ByteString
        , prdCountry :: FieldUse ByteString
        , prdState :: FieldUse ByteString
        , prdSetting :: FieldUse ByteString
        } 
    deriving (Show, Eq, Ord)

data Measure = Measure
  { measureRecord :: Record
  , measureMetric :: Metric
  , measureDatum :: !MeasureDatum
  }

instance Kinded Measure where
  kindOf _ = "measure"

-- TODO: example building circular Record + Measure

type Measures = [Measure]

-- makeHasRec ''RecordRow ['recordId, 'recordCategory]
-- makeHasRec ''Record ['recordRow, 'recordVolume, 'recordRelease]
instance Has (Id Record) RecordRow where
  view = recordId
instance Has Category RecordRow where
  view = recordCategory
instance Has (Id Category) RecordRow where
  view = (view . recordCategory)

-- instance Has RecordRow Record where
--   view = recordRow
instance Has (Id Record) Record where
  view = (view . recordRow)
instance Has Category Record where
  view = (view . recordRow)
instance Has (Id Category) Record where
  view = (view . recordRow)
instance Has Volume Record where
  view = recordVolume
instance Has Permission Record where
  view = (view . recordVolume)
instance Has (Id Volume) Record where
  view = (view . recordVolume)
-- instance Has VolumeRow Record where
--   view = (view . recordVolume)
instance Has (Maybe Release) Record where
  view = recordRelease
instance Has Release Record where
  view = (view . recordRelease)

getRecordVolumePermission :: Record -> (Permission, VolumeAccessPolicy)
getRecordVolumePermission = volumePermissionPolicy . recordVolume

instance Has Record Measure where
  view = measureRecord
-- instance Has (Id Record) Measure where
--   view = recordId . recordRow . measureRecord
-- instance Has Volume Measure where
--   view = view . measureRecord
-- instance Has (Id Volume) Measure where
--   view = view . measureRecord
-- instance Has Category Measure where
--   view = view . measureRecord
-- instance Has (Id Category) Measure where
---  view = view . measureRecord
-- instance Has Permission Measure where
--    view = view . measureRecord

-- instance Has Metric Measure where
--   view = measureMetric
-- instance Has (Id Metric) Measure where
--   view = view . measureMetric
-- instance Has MeasureType Measure where
--   view = view . measureMetric

instance Has (Maybe Release) Measure where
  view m = metricRelease (measureMetric m) <|> recordRelease (measureRecord m)
instance Has Release Measure where
  view = fold . (view :: Measure -> Maybe Release)

blankRecord :: Category -> Volume -> Record
blankRecord cat vol = Record
  { recordRow = RecordRow
    { recordId = error "blankRecord"
    , recordCategory = cat
    }
  , recordVolume = vol
  , recordRelease = Nothing
  , recordMeasures = []
  }

