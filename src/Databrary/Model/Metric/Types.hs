{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable, OverloadedStrings, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Metric.Types
  ( MeasureDatum
  , MeasureType(..)
  , Metric(..)
  , ParticipantFieldMapping(..)
  -- for tests
  , testMetric1
  , testMeasureType1
  ) where

import qualified Data.ByteString as BS
import Data.Function (on)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Text (Text)
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLiftMany)
import qualified Data.Typeable.Internal
import qualified GHC.Arr
import qualified Database.PostgreSQL.Typed.Types
import qualified Database.PostgreSQL.Typed.Dynamic
import qualified Database.PostgreSQL.Typed.Enum
import qualified Data.Aeson.Types
import qualified Data.ByteString
import qualified Data.ByteString.Char8

import Databrary.Has (makeHasRec)
import Databrary.Model.Enum
import Databrary.Model.Kind
import Databrary.Model.Release.Types
import Databrary.Model.Id.Types
import Databrary.Model.Category.Types
import qualified Databrary.Model.Kind
import qualified Databrary.HTTP.Form.Deform

-- makeDBEnum "data_type" "MeasureType"
-- TODO: db coherence
data MeasureType
  = MeasureTypeText |
    MeasureTypeNumeric |
    MeasureTypeDate |
    MeasureTypeVoid
  deriving (Eq,
            Ord,
            Enum,
            GHC.Arr.Ix,
            Bounded,
            Data.Typeable.Internal.Typeable)
instance Show MeasureType where
  show MeasureTypeText = "text"
  show MeasureTypeNumeric = "numeric"
  show MeasureTypeDate = "date"
  show MeasureTypeVoid = "void"
instance Database.PostgreSQL.Typed.Types.PGType "data_type"
instance Database.PostgreSQL.Typed.Types.PGParameter "data_type" MeasureType where
  pgEncode _ MeasureTypeText
    = BS.pack [116, 101, 120, 116]
  pgEncode _ MeasureTypeNumeric
    = BS.pack [110, 117, 109, 101, 114, 105, 99]
  pgEncode _ MeasureTypeDate
    = BS.pack [100, 97, 116, 101]
  pgEncode _ MeasureTypeVoid
    = BS.pack [118, 111, 105, 100]
instance Database.PostgreSQL.Typed.Types.PGColumn "data_type" MeasureType where
  pgDecode _ x_a4zCt
    = case BS.unpack x_a4zCt of 
        [116, 101, 120, 116] -> MeasureTypeText
        [110, 117, 109, 101, 114, 105, 99] -> MeasureTypeNumeric
        [100, 97, 116, 101] -> MeasureTypeDate
        [118, 111, 105, 100] -> MeasureTypeVoid
        _ -> error
               ("pgDecode data_type: "
                ++ (Data.ByteString.Char8.unpack x_a4zCt)) 
instance Database.PostgreSQL.Typed.Dynamic.PGRep "data_type" MeasureType
instance Database.PostgreSQL.Typed.Enum.PGEnum MeasureType
instance Kinded MeasureType where
  kindOf _ = "data_type"
instance DBEnum MeasureType
instance Data.Aeson.Types.ToJSON MeasureType where
  toJSON
    = (Data.Aeson.Types.toJSON . fromEnum)
instance Data.Aeson.Types.FromJSON MeasureType where
  parseJSON = parseJSONEnum
instance Databrary.HTTP.Form.Deform.Deform f_a4zCu MeasureType where
  deform = enumForm

type MeasureDatum = BS.ByteString

type instance IdType Metric = Int32

data Metric = Metric
  { metricId :: !(Id Metric)
  , metricCategory :: !Category
  , metricName :: !T.Text
  , metricRelease :: !(Maybe Release)
  , metricType :: !MeasureType
  , metricOptions :: ![MeasureDatum]
  , metricAssumed :: !(Maybe MeasureDatum)
  , metricDescription :: !(Maybe T.Text)
  , metricRequired :: !(Maybe Bool)
  }

instance Kinded Metric where
  kindOf _ = "metric"

instance Eq Metric where
  (==) = on (==) metricId
  (/=) = on (/=) metricId

instance Ord Metric where
  compare = comparing metricId

makeHasRec ''Metric ['metricId, 'metricCategory, 'metricRelease, 'metricType]
deriveLiftMany [''MeasureType, ''Metric]

data ParticipantFieldMapping =  -- each field can be nothing = not used, or just "colname" for csv column
    ParticipantFieldMapping -- are all of these maybe or are some required?
        { pfmId :: Maybe Text  -- TODO: strict fields
        , pfmInfo :: Maybe Text
        , pfmDescription :: Maybe Text
        , pfmBirthdate :: Maybe Text
        , pfmGender :: Maybe Text
        , pfmRace :: Maybe Text
        , pfmEthnicity :: Maybe Text
        , pfmGestationalAge :: Maybe Text
        , pfmPregnancyTerm :: Maybe Text
        , pfmBirthWeight :: Maybe Text
        , pfmDisability :: Maybe Text
        , pfmLanguage :: Maybe Text
        , pfmCountry :: Maybe Text
        , pfmState :: Maybe Text
        , pfmSetting :: Maybe Text
        } 
    deriving (Show, Eq, Ord)

testMeasureType1 :: MeasureType
testMeasureType1 = MeasureTypeText

testMetric1 :: Metric
testMetric1 =
  Metric {
      metricId = Id (-900)
    , metricCategory = testCategory1
    , metricName = "ID"
    , metricRelease = Just ReleaseEXCERPTS
    , metricType = MeasureTypeText
    , metricOptions = []
    , metricAssumed = Nothing
    , metricDescription = Nothing -- where does this come from?
    , metricRequired = Nothing -- where does this come from?
  }
