
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}


module Model.Record.Types
  ( RecordRow(..)
  , Record(..)
  , ParticipantRecord(..)
  , FieldUse(..)
  , getRecordVolumePermission
  , Measure(..)
  , Measures
  , blankRecord
  ) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.Time (Day)

import Has (Has(..))
import Model.Kind
import Model.Id.Types
import Model.Permission.Types
import Model.Release.Types
import Model.Volume.Types
import Model.Metric.Types
import Model.Category.Types

type instance IdType Record = Int32

data RecordRow = RecordRow
  { recordId :: Id Record
  , recordCategory :: Category
  }

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
    -- deriving (Show, Eq, Ord, Functor)

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

data Measure = Measure
  { measureRecord :: Record
  , measureMetric :: Metric
  , measureDatum :: !MeasureDatum
  }

-- instance Kinded Measure where
--   kindOf _ = "measure"

-- TODO: example building circular Record + Measure

type Measures = [Measure]

instance Has (Id Record) Record where
  view = recordId . recordRow
instance Has Category Record where
  view = recordCategory . recordRow
instance Has (Id Category) Record where
  view = categoryId . recordCategory . recordRow
instance Has Volume Record where
  view = recordVolume
instance Has Permission Record where
  view = view . recordVolume
instance Has (Maybe Release) Record where
  view = recordRelease
instance Has Release Record where
  view = view . recordRelease

getRecordVolumePermission :: Record -> VolumeRolePolicy
getRecordVolumePermission = volumeRolePolicy . recordVolume

instance Has Record Measure where
  view = measureRecord

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

