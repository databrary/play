{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Databrary.Model.Record.Types
  ( RecordRow(..)
  , Record(..)
  , ParticipantRecord(..)
  , getRecordVolumePermission
  , Measure(..)
  , Measures
  , blankRecord
  -- for tests
  , testRecordRow1
  ) where

import Control.Applicative ((<|>))

import Databrary.Has (makeHasRec, Has(..))
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

testRecordRow1 :: RecordRow
testRecordRow1 =
    RecordRow {
        recordId = Id 100
      , recordCategory = testCategory1
    }

data Record = Record
  { recordRow :: !RecordRow
  , recordMeasures :: Measures
  , recordRelease :: Maybe Release
  , recordVolume :: Volume
  }

instance Kinded Record where
  kindOf _ = "record"

data ParticipantRecord = -- each field can be nothing (not used) or just value
    ParticipantRecord -- are some of these required?
        { prdId :: !(Maybe MeasureDatum)
        , prdInfo :: !(Maybe MeasureDatum)
        , prdDescription :: !(Maybe MeasureDatum)
        , prdBirthdate :: !(Maybe MeasureDatum)
        , prdGender :: !(Maybe MeasureDatum)
        , prdRace :: !(Maybe MeasureDatum)
        , prdEthnicity :: !(Maybe MeasureDatum)
        , prdGestationalAge :: !(Maybe MeasureDatum)
        , prdPregnancyTerm :: !(Maybe MeasureDatum)
        , prdBirthWeight :: !(Maybe MeasureDatum)
        , prdDisability :: !(Maybe MeasureDatum)
        , prdLanguage :: !(Maybe MeasureDatum)
        , prdCountry :: !(Maybe MeasureDatum)
        , prdState :: !(Maybe MeasureDatum)
        , prdSetting :: !(Maybe MeasureDatum)
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

makeHasRec ''RecordRow ['recordId, 'recordCategory]
makeHasRec ''Record ['recordRow, 'recordVolume, 'recordRelease]
getRecordVolumePermission :: Record -> (Permission, VolumeAccessPolicy)
getRecordVolumePermission = volumePermissionPolicy . recordVolume

instance Has Record Measure where
  view = measureRecord
instance Has (Id Record) Measure where
  view = view . measureRecord
instance Has Volume Measure where
  view = view . measureRecord
instance Has (Id Volume) Measure where
  view = view . measureRecord
instance Has Category Measure where
  view = view . measureRecord
instance Has (Id Category) Measure where
  view = view . measureRecord
instance Has Permission Measure where
  view = view . measureRecord

instance Has Metric Measure where
  view = measureMetric
instance Has (Id Metric) Measure where
  view = view . measureMetric
instance Has MeasureType Measure where
  view = view . measureMetric

instance Has (Maybe Release) Measure where
  view m = metricRelease (measureMetric m) <|> recordRelease (measureRecord m)
instance Has Release Measure where
  view = view . (view :: Measure -> Maybe Release)

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

