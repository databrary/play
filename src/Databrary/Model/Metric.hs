{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, DataKinds #-}
module Databrary.Model.Metric
  ( module Databrary.Model.Metric.Types
  , allMetrics
  , participantMetricId
  , participantMetricInfo
  , participantMetricDescription
  , participantMetricBirthdate
  , participantMetricGender
  , participantMetricRace
  , participantMetricEthnicity
  , participantMetricGestationalAge
  , participantMetricPregnancyTerm
  , participantMetricBirthWeight
  , participantMetricDisability
  , participantMetricLanguage
  , participantMetricCountry
  , participantMetricState
  , participantMetricSetting
  , getMetric
  , getMetric'
  , lookupParticipantMetricBySymbolicName
  , participantMetrics
  , validateParticipantId
  , validateParticipantInfo
  , validateParticipantDescription
  , validateParticipantGender
  , validateParticipantCountry
  , validateParticipantRace
  , validateParticipantEthnicity
  , validateParticipantPregnancyTerm
  , validateParticipantState
  , validateParticipantSetting
  , validateParticipantDisability
  , validateParticipantGestationalAge
  , validateParticipantBirthWeight
  , validateParticipantBirthdate
  , validateParticipantLanguage
  , metricLong
  , birthdateMetric
  , metricJSON
  ) where

import Control.Applicative (empty, pure)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString
import qualified Data.IntMap.Strict as IntMap
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.Text
import Data.Text (Text)
import qualified Text.Read as TR

import Databrary.Ops
import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Category
import Databrary.Model.Metric.Types
import qualified Databrary.Model.Release.Types

-- TODO: db coherence
allMetrics :: [Metric]
allMetrics = 
    [Metric
       (Id 1)
       (Category
          (Id 1)
          (Data.Text.pack "participant")
          (Just
             (Data.Text.pack
                "An individual human subject whose data are used or represented")))
       (Data.Text.pack "ID")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       Nothing
       (Just
          (Data.Text.pack
             "A unique, anonymized, primary identifier, such as participant ID"))
       (Just True),
     Metric
       (Id 2)
       (Category
          (Id 1)
          (Data.Text.pack "participant")
          (Just
             (Data.Text.pack
                "An individual human subject whose data are used or represented")))
       (Data.Text.pack "info")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       Nothing
       (Just (Data.Text.pack "Other information or alternate identifier"))
       Nothing,
     Metric
       (Id 3)
       (Category
          (Id 1)
          (Data.Text.pack "participant")
          (Just
             (Data.Text.pack
                "An individual human subject whose data are used or represented")))
       (Data.Text.pack "description")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       Nothing
       (Just (Data.Text.pack "A longer explanation or description"))
       Nothing,
     Metric
       (Id 4)
       (Category
          (Id 1)
          (Data.Text.pack "participant")
          (Just
             (Data.Text.pack
                "An individual human subject whose data are used or represented")))
       (Data.Text.pack "birthdate")
       Nothing
       MeasureTypeDate
       []
       Nothing
       (Just
          (Data.Text.pack
             "Date of birth (used with session date to calculate age; you can also use the group category to designate age groups)"))
       (Just False),
     Metric
       (Id 5)
       (Category
          (Id 1)
          (Data.Text.pack "participant")
          (Just
             (Data.Text.pack
                "An individual human subject whose data are used or represented")))
       (Data.Text.pack "gender")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       [Data.ByteString.pack [70, 101, 109, 97, 108, 101],
        Data.ByteString.pack [77, 97, 108, 101]]
       Nothing
       (Just
          (Data.Text.pack
             "\"Male\", \"Female\", or any other relevant gender"))
       (Just False),
     Metric
       (Id 6)
       (Category
          (Id 1)
          (Data.Text.pack "participant")
          (Just
             (Data.Text.pack
                "An individual human subject whose data are used or represented")))
       (Data.Text.pack "race")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       [Data.ByteString.pack
          [65, 109, 101, 114, 105, 99, 97, 110, 32, 73, 110, 100, 105, 97,
           110, 32, 111, 114, 32, 65, 108, 97, 115, 107, 97, 32, 78, 97, 116,
           105, 118, 101],
        Data.ByteString.pack [65, 115, 105, 97, 110],
        Data.ByteString.pack
          [78, 97, 116, 105, 118, 101, 32, 72, 97, 119, 97, 105, 105, 97,
           110, 32, 111, 114, 32, 79, 116, 104, 101, 114, 32, 80, 97, 99, 105,
           102, 105, 99, 32, 73, 115, 108, 97, 110, 100, 101, 114],
        Data.ByteString.pack
          [66, 108, 97, 99, 107, 32, 111, 114, 32, 65, 102, 114, 105, 99, 97,
           110, 32, 65, 109, 101, 114, 105, 99, 97, 110],
        Data.ByteString.pack [87, 104, 105, 116, 101],
        Data.ByteString.pack
          [77, 111, 114, 101, 32, 116, 104, 97, 110, 32, 111, 110, 101],
        Data.ByteString.pack
          [85, 110, 107, 110, 111, 119, 110, 32, 111, 114, 32, 110, 111, 116,
           32, 114, 101, 112, 111, 114, 116, 101, 100]]
       Nothing
       (Just
          (Data.Text.pack
             "As classified by NIH, or user-defined classification"))
       (Just False),
     Metric
       (Id 7)
       (Category
          (Id 1)
          (Data.Text.pack "participant")
          (Just
             (Data.Text.pack
                "An individual human subject whose data are used or represented")))
       (Data.Text.pack "ethnicity")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       [Data.ByteString.pack
          [78, 111, 116, 32, 72, 105, 115, 112, 97, 110, 105, 99, 32, 111,
           114, 32, 76, 97, 116, 105, 110, 111],
        Data.ByteString.pack
          [72, 105, 115, 112, 97, 110, 105, 99, 32, 111, 114, 32, 76, 97,
           116, 105, 110, 111],
        Data.ByteString.pack
          [85, 110, 107, 110, 111, 119, 110, 32, 111, 114, 32, 110, 111, 116,
           32, 114, 101, 112, 111, 114, 116, 101, 100]]
       Nothing
       (Just
          (Data.Text.pack
             "As classified by NIH (Hispanic/Non-Hispanic), or user-defined classification"))
       (Just False),
     Metric
       (Id 8)
       (Category
          (Id 1)
          (Data.Text.pack "participant")
          (Just
             (Data.Text.pack
                "An individual human subject whose data are used or represented")))
       (Data.Text.pack "gestational age")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeNumeric
       []
       Nothing
       (Just
          (Data.Text.pack
             "Pregnancy age in weeks between last menstrual period and birth (or pre-natal observation)"))
       Nothing,
     Metric
       (Id 9)
       (Category
          (Id 1)
          (Data.Text.pack "participant")
          (Just
             (Data.Text.pack
                "An individual human subject whose data are used or represented")))
       (Data.Text.pack "pregnancy term")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       [Data.ByteString.pack [70, 117, 108, 108, 32, 116, 101, 114, 109],
        Data.ByteString.pack [80, 114, 101, 116, 101, 114, 109]]
       (Just
          (Data.ByteString.pack [70, 117, 108, 108, 32, 116, 101, 114, 109]))
       (Just
          (Data.Text.pack
             "\"Full term\", \"Preterm\", or other gestational term"))
       Nothing,
     Metric
       (Id 10)
       (Category
          (Id 1)
          (Data.Text.pack "participant")
          (Just
             (Data.Text.pack
                "An individual human subject whose data are used or represented")))
       (Data.Text.pack "birth weight")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeNumeric
       []
       Nothing
       (Just (Data.Text.pack "Weight at birth (in grams, e.g., 3250)"))
       Nothing,
     Metric
       (Id 11)
       (Category
          (Id 1)
          (Data.Text.pack "participant")
          (Just
             (Data.Text.pack
                "An individual human subject whose data are used or represented")))
       (Data.Text.pack "disability")
       Nothing
       MeasureTypeText
       []
       (Just (Data.ByteString.pack [116, 121, 112, 105, 99, 97, 108]))
       (Just
          (Data.Text.pack
             "Any developmental, physical, or mental disability or disabilities"))
       (Just False),
     Metric
       (Id 12)
       (Category
          (Id 1)
          (Data.Text.pack "participant")
          (Just
             (Data.Text.pack
                "An individual human subject whose data are used or represented")))
       (Data.Text.pack "language")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       (Just (Data.ByteString.pack [69, 110, 103, 108, 105, 115, 104]))
       (Just
          (Data.Text.pack
             "Primary language(s) spoken by and to participant"))
       (Just False),
     Metric
       (Id 13)
       (Category
          (Id 1)
          (Data.Text.pack "participant")
          (Just
             (Data.Text.pack
                "An individual human subject whose data are used or represented")))
       (Data.Text.pack "country")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       (Just (Data.ByteString.pack [85, 83]))
       (Just (Data.Text.pack "Country where participant was born"))
       Nothing,
     Metric
       (Id 14)
       (Category
          (Id 1)
          (Data.Text.pack "participant")
          (Just
             (Data.Text.pack
                "An individual human subject whose data are used or represented")))
       (Data.Text.pack "state")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       [Data.ByteString.pack [65, 76], Data.ByteString.pack [65, 75],
        Data.ByteString.pack [65, 90], Data.ByteString.pack [65, 82],
        Data.ByteString.pack [67, 65], Data.ByteString.pack [67, 79],
        Data.ByteString.pack [67, 84], Data.ByteString.pack [68, 69],
        Data.ByteString.pack [68, 67], Data.ByteString.pack [70, 76],
        Data.ByteString.pack [71, 65], Data.ByteString.pack [72, 73],
        Data.ByteString.pack [73, 68], Data.ByteString.pack [73, 76],
        Data.ByteString.pack [73, 78], Data.ByteString.pack [73, 65],
        Data.ByteString.pack [75, 83], Data.ByteString.pack [75, 89],
        Data.ByteString.pack [76, 65], Data.ByteString.pack [77, 69],
        Data.ByteString.pack [77, 84], Data.ByteString.pack [78, 69],
        Data.ByteString.pack [78, 86], Data.ByteString.pack [78, 72],
        Data.ByteString.pack [78, 74], Data.ByteString.pack [78, 77],
        Data.ByteString.pack [78, 89], Data.ByteString.pack [78, 67],
        Data.ByteString.pack [78, 68], Data.ByteString.pack [79, 72],
        Data.ByteString.pack [79, 75], Data.ByteString.pack [79, 82],
        Data.ByteString.pack [77, 68], Data.ByteString.pack [77, 65],
        Data.ByteString.pack [77, 73], Data.ByteString.pack [77, 78],
        Data.ByteString.pack [77, 83], Data.ByteString.pack [77, 79],
        Data.ByteString.pack [80, 65], Data.ByteString.pack [82, 73],
        Data.ByteString.pack [83, 67], Data.ByteString.pack [83, 68],
        Data.ByteString.pack [84, 78], Data.ByteString.pack [84, 88],
        Data.ByteString.pack [85, 84], Data.ByteString.pack [86, 84],
        Data.ByteString.pack [86, 65], Data.ByteString.pack [87, 65],
        Data.ByteString.pack [87, 86], Data.ByteString.pack [87, 73],
        Data.ByteString.pack [87, 89]]
       Nothing
       (Just
          (Data.Text.pack "State/territory where participant was born"))
       Nothing,
     Metric
       (Id 15)
       (Category
          (Id 1)
          (Data.Text.pack "participant")
          (Just
             (Data.Text.pack
                "An individual human subject whose data are used or represented")))
       (Data.Text.pack "setting")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       [Data.ByteString.pack [76, 97, 98],
        Data.ByteString.pack [72, 111, 109, 101],
        Data.ByteString.pack [67, 108, 97, 115, 115, 114, 111, 111, 109],
        Data.ByteString.pack [79, 117, 116, 100, 111, 111, 114],
        Data.ByteString.pack [67, 108, 105, 110, 105, 99]]
       Nothing
       (Just
          (Data.Text.pack
             "The physical context of the participant (please do not use for new data: see the context category instead)"))
       Nothing,
     Metric
       (Id 16)
       (Category
          (Id 2)
          (Data.Text.pack "pilot")
          (Just
             (Data.Text.pack
                "Indicates that the methods used were not finalized or were non-standard")))
       (Data.Text.pack "pilot")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeVoid
       []
       Nothing
       Nothing
       (Just False),
     Metric
       (Id 17)
       (Category
          (Id 2)
          (Data.Text.pack "pilot")
          (Just
             (Data.Text.pack
                "Indicates that the methods used were not finalized or were non-standard")))
       (Data.Text.pack "name")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       Nothing
       (Just
          (Data.Text.pack
             "A label or identifier referring to the pilot method"))
       Nothing,
     Metric
       (Id 18)
       (Category
          (Id 2)
          (Data.Text.pack "pilot")
          (Just
             (Data.Text.pack
                "Indicates that the methods used were not finalized or were non-standard")))
       (Data.Text.pack "description")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       Nothing
       (Just
          (Data.Text.pack
             "A longer explanation or description of the pilot method"))
       Nothing,
     Metric
       (Id 19)
       (Category
          (Id 3)
          (Data.Text.pack "exclusion")
          (Just (Data.Text.pack "Indicates that data were not usable")))
       (Data.Text.pack "excluded")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeVoid
       []
       Nothing
       Nothing
       Nothing,
     Metric
       (Id 20)
       (Category
          (Id 3)
          (Data.Text.pack "exclusion")
          (Just (Data.Text.pack "Indicates that data were not usable")))
       (Data.Text.pack "name")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       Nothing
       (Just
          (Data.Text.pack
             "A label or identifier referring to the exclusion criterion"))
       Nothing,
     Metric
       (Id 21)
       (Category
          (Id 3)
          (Data.Text.pack "exclusion")
          (Just (Data.Text.pack "Indicates that data were not usable")))
       (Data.Text.pack "reason")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       [Data.ByteString.pack
          [68, 105, 100, 32, 110, 111, 116, 32, 109, 101, 101, 116, 32, 105,
           110, 99, 108, 117, 115, 105, 111, 110, 32, 99, 114, 105, 116, 101,
           114, 105, 97],
        Data.ByteString.pack
          [80, 114, 111, 99, 101, 100, 117, 114, 97, 108, 47, 101, 120, 112,
           101, 114, 105, 109, 101, 110, 116, 101, 114, 32, 101, 114, 114,
           111, 114],
        Data.ByteString.pack
          [87, 105, 116, 104, 100, 114, 101, 119, 47, 102, 117, 115, 115,
           121, 47, 116, 105, 114, 101, 100],
        Data.ByteString.pack [79, 117, 116, 108, 105, 101, 114]]
       Nothing
       (Just (Data.Text.pack "The reason for excluding these data"))
       (Just False),
     Metric
       (Id 22)
       (Category
          (Id 3)
          (Data.Text.pack "exclusion")
          (Just (Data.Text.pack "Indicates that data were not usable")))
       (Data.Text.pack "description")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       Nothing
       (Just
          (Data.Text.pack
             "A longer explanation or description of the reason for excluding data"))
       Nothing,
     Metric
       (Id 23)
       (Category
          (Id 4)
          (Data.Text.pack "condition")
          (Just
             (Data.Text.pack
                "An experimenter-determined manipulation (within or between sessions)")))
       (Data.Text.pack "name")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       Nothing
       (Just (Data.Text.pack "A label or identifier for the condition"))
       (Just True),
     Metric
       (Id 24)
       (Category
          (Id 4)
          (Data.Text.pack "condition")
          (Just
             (Data.Text.pack
                "An experimenter-determined manipulation (within or between sessions)")))
       (Data.Text.pack "description")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       Nothing
       (Just
          (Data.Text.pack
             "A longer explanation or description of the condition"))
       Nothing,
     Metric
       (Id 25)
       (Category
          (Id 4)
          (Data.Text.pack "condition")
          (Just
             (Data.Text.pack
                "An experimenter-determined manipulation (within or between sessions)")))
       (Data.Text.pack "info")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       Nothing
       (Just (Data.Text.pack "Other information or alternate identifier"))
       Nothing,
     Metric
       (Id 26)
       (Category
          (Id 5)
          (Data.Text.pack "group")
          (Just
             (Data.Text.pack
                "A grouping determined by an aspect of the data (participant ability, age, grade level, experience, longitudinal visit, measurements used/available)")))
       (Data.Text.pack "name")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       Nothing
       (Just (Data.Text.pack "A label or identifier for the grouping"))
       (Just True),
     Metric
       (Id 27)
       (Category
          (Id 5)
          (Data.Text.pack "group")
          (Just
             (Data.Text.pack
                "A grouping determined by an aspect of the data (participant ability, age, grade level, experience, longitudinal visit, measurements used/available)")))
       (Data.Text.pack "description")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       Nothing
       (Just
          (Data.Text.pack
             "A longer explanation or description of the grouping"))
       Nothing,
     Metric
       (Id 28)
       (Category
          (Id 5)
          (Data.Text.pack "group")
          (Just
             (Data.Text.pack
                "A grouping determined by an aspect of the data (participant ability, age, grade level, experience, longitudinal visit, measurements used/available)")))
       (Data.Text.pack "info")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       Nothing
       (Just (Data.Text.pack "Other information or alternate identifier"))
       Nothing,
     Metric
       (Id 29)
       (Category
          (Id 6)
          (Data.Text.pack "task")
          (Just
             (Data.Text.pack
                "A particular task, activity, or phase of the session or study")))
       (Data.Text.pack "name")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       Nothing
       (Just (Data.Text.pack "A label or identifier for the task"))
       (Just True),
     Metric
       (Id 30)
       (Category
          (Id 6)
          (Data.Text.pack "task")
          (Just
             (Data.Text.pack
                "A particular task, activity, or phase of the session or study")))
       (Data.Text.pack "description")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       Nothing
       (Just
          (Data.Text.pack "A longer explanation or description of the task"))
       (Just False),
     Metric
       (Id 31)
       (Category
          (Id 6)
          (Data.Text.pack "task")
          (Just
             (Data.Text.pack
                "A particular task, activity, or phase of the session or study")))
       (Data.Text.pack "info")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       Nothing
       (Just (Data.Text.pack "Other information or alternate identifier"))
       Nothing,
     Metric
       (Id 32)
       (Category
          (Id 7)
          (Data.Text.pack "context")
          (Just
             (Data.Text.pack
                "A particular setting or other aspect of where/when/how data were collected")))
       (Data.Text.pack "name")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       Nothing
       (Just (Data.Text.pack "A label or identifier for the context"))
       Nothing,
     Metric
       (Id 33)
       (Category
          (Id 7)
          (Data.Text.pack "context")
          (Just
             (Data.Text.pack
                "A particular setting or other aspect of where/when/how data were collected")))
       (Data.Text.pack "setting")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       [Data.ByteString.pack [76, 97, 98],
        Data.ByteString.pack [72, 111, 109, 101],
        Data.ByteString.pack [67, 108, 97, 115, 115, 114, 111, 111, 109],
        Data.ByteString.pack [79, 117, 116, 100, 111, 111, 114],
        Data.ByteString.pack [67, 108, 105, 110, 105, 99]]
       Nothing
       (Just (Data.Text.pack "The physical context"))
       (Just True),
     Metric
       (Id 34)
       (Category
          (Id 7)
          (Data.Text.pack "context")
          (Just
             (Data.Text.pack
                "A particular setting or other aspect of where/when/how data were collected")))
       (Data.Text.pack "language")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       (Just (Data.ByteString.pack [69, 110, 103, 108, 105, 115, 104]))
       (Just (Data.Text.pack "Language used in this context"))
       (Just False),
     Metric
       (Id 35)
       (Category
          (Id 7)
          (Data.Text.pack "context")
          (Just
             (Data.Text.pack
                "A particular setting or other aspect of where/when/how data were collected")))
       (Data.Text.pack "country")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       []
       (Just (Data.ByteString.pack [85, 83]))
       (Just (Data.Text.pack "Country of data collection"))
       (Just False),
     Metric
       (Id 36)
       (Category
          (Id 7)
          (Data.Text.pack "context")
          (Just
             (Data.Text.pack
                "A particular setting or other aspect of where/when/how data were collected")))
       (Data.Text.pack "state")
       (Just Databrary.Model.Release.Types.ReleasePUBLIC)
       MeasureTypeText
       [Data.ByteString.pack [65, 76], Data.ByteString.pack [65, 75],
        Data.ByteString.pack [65, 90], Data.ByteString.pack [65, 82],
        Data.ByteString.pack [67, 65], Data.ByteString.pack [67, 79],
        Data.ByteString.pack [67, 84], Data.ByteString.pack [68, 69],
        Data.ByteString.pack [68, 67], Data.ByteString.pack [70, 76],
        Data.ByteString.pack [71, 65], Data.ByteString.pack [72, 73],
        Data.ByteString.pack [73, 68], Data.ByteString.pack [73, 76],
        Data.ByteString.pack [73, 78], Data.ByteString.pack [73, 65],
        Data.ByteString.pack [75, 83], Data.ByteString.pack [75, 89],
        Data.ByteString.pack [76, 65], Data.ByteString.pack [77, 69],
        Data.ByteString.pack [77, 84], Data.ByteString.pack [78, 69],
        Data.ByteString.pack [78, 86], Data.ByteString.pack [78, 72],
        Data.ByteString.pack [78, 74], Data.ByteString.pack [78, 77],
        Data.ByteString.pack [78, 89], Data.ByteString.pack [78, 67],
        Data.ByteString.pack [78, 68], Data.ByteString.pack [79, 72],
        Data.ByteString.pack [79, 75], Data.ByteString.pack [79, 82],
        Data.ByteString.pack [77, 68], Data.ByteString.pack [77, 65],
        Data.ByteString.pack [77, 73], Data.ByteString.pack [77, 78],
        Data.ByteString.pack [77, 83], Data.ByteString.pack [77, 79],
        Data.ByteString.pack [80, 65], Data.ByteString.pack [82, 73],
        Data.ByteString.pack [83, 67], Data.ByteString.pack [83, 68],
        Data.ByteString.pack [84, 78], Data.ByteString.pack [84, 88],
        Data.ByteString.pack [85, 84], Data.ByteString.pack [86, 84],
        Data.ByteString.pack [86, 65], Data.ByteString.pack [87, 65],
        Data.ByteString.pack [87, 86], Data.ByteString.pack [87, 73],
        Data.ByteString.pack [87, 89]]
       Nothing
       (Just (Data.Text.pack "State/territory of data collection"))
       (Just False)]

participantMetricId :: Metric
participantMetricId = getMetric' (Id 1)
  
participantMetricInfo :: Metric
participantMetricInfo = getMetric' (Id 2)

participantMetricDescription :: Metric
participantMetricDescription = getMetric' (Id 3)

participantMetricBirthdate :: Metric
participantMetricBirthdate = getMetric' (Id 4)

participantMetricGender :: Metric
participantMetricGender = getMetric' (Id 5)

participantMetricRace :: Metric
participantMetricRace = getMetric' (Id 6)

participantMetricEthnicity :: Metric
participantMetricEthnicity = getMetric' (Id 7)

participantMetricGestationalAge :: Metric
participantMetricGestationalAge = getMetric' (Id 8)

participantMetricPregnancyTerm :: Metric
participantMetricPregnancyTerm = getMetric' (Id 9)

participantMetricBirthWeight :: Metric
participantMetricBirthWeight = getMetric' (Id 10)

participantMetricDisability :: Metric
participantMetricDisability = getMetric' (Id 11)

participantMetricLanguage :: Metric
participantMetricLanguage = getMetric' (Id 12)

participantMetricCountry :: Metric
participantMetricCountry = getMetric' (Id 13)

participantMetricState :: Metric
participantMetricState = getMetric' (Id 14)

participantMetricSetting :: Metric
participantMetricSetting = getMetric' (Id 15)

metricsById :: IntMap.IntMap Metric
metricsById = IntMap.fromAscList $ map (\a -> (fromIntegral $ unId $ metricId a, a)) allMetrics

getMetric :: Id Metric -> Maybe Metric
getMetric (Id i) = IntMap.lookup (fromIntegral i) metricsById

getMetric' :: Id Metric -> Metric
getMetric' (Id i) = metricsById IntMap.! fromIntegral i

lookupParticipantMetricBySymbolicName :: Text -> Maybe Metric
lookupParticipantMetricBySymbolicName symbolicName =
    find (\m -> (Data.Text.filter (/= ' ') . Data.Text.toLower . metricName) m == symbolicName) participantMetrics

participantMetrics :: [Metric]
participantMetrics = filter ((== participantCategory) . metricCategory) allMetrics

validateParticipantId :: BS.ByteString -> Maybe BS.ByteString
validateParticipantId val = validateNotEmpty val

validateParticipantInfo :: BS.ByteString -> Maybe BS.ByteString
validateParticipantInfo val = pure val

validateParticipantDescription :: BS.ByteString -> Maybe BS.ByteString
validateParticipantDescription val = pure val

validateParticipantDisability :: BS.ByteString -> Maybe BS.ByteString
validateParticipantDisability val = pure val

validateParticipantGender :: BS.ByteString -> Maybe BS.ByteString
validateParticipantGender val =
    validateInOptions val participantMetricGender

validateParticipantCountry :: BS.ByteString -> Maybe BS.ByteString
validateParticipantCountry val = pure val

validateParticipantRace :: BS.ByteString -> Maybe BS.ByteString
validateParticipantRace val =
    validateInOptions val participantMetricRace

validateParticipantEthnicity :: BS.ByteString -> Maybe BS.ByteString
validateParticipantEthnicity val =
    validateInOptions val participantMetricEthnicity

validateParticipantPregnancyTerm :: BS.ByteString -> Maybe BS.ByteString
validateParticipantPregnancyTerm val =
    validateInOptions val participantMetricPregnancyTerm

validateParticipantState :: BS.ByteString -> Maybe BS.ByteString
validateParticipantState val =
    validateInOptions val participantMetricState

validateParticipantSetting :: BS.ByteString -> Maybe BS.ByteString
validateParticipantSetting val =
    validateInOptions val participantMetricSetting

validateParticipantGestationalAge :: BS.ByteString -> Maybe BS.ByteString
validateParticipantGestationalAge val = do
    _ <- (TR.readMaybe (BSC.unpack val) :: Maybe Double)
    pure val

validateParticipantBirthWeight :: BS.ByteString -> Maybe BS.ByteString
validateParticipantBirthWeight val = do
    _ <- (TR.readMaybe (BSC.unpack val) :: Maybe Double)
    pure val

validateParticipantBirthdate :: BS.ByteString -> Maybe BS.ByteString
validateParticipantBirthdate val = do
    -- TODO: which date format??
    -- _ <- (TR.readMaybe (BSC.unpack val) :: Maybe Double)
    -- pure val
    pure val

validateParticipantLanguage :: BS.ByteString -> Maybe BS.ByteString
validateParticipantLanguage val = do
    pure val

validateInOptions :: BS.ByteString -> Metric -> Maybe BS.ByteString
validateInOptions val metric =
    if val == ""
    then Just ""
    else find (== val) (metricOptions metric)

validateNotEmpty :: BS.ByteString -> Maybe BS.ByteString
validateNotEmpty val =
    if BS.length val > 0 then Just val else Nothing

-- this is a hack, should be in database
metricLong :: Metric -> Bool
metricLong = ("description" ==) . metricName

birthdateMetric :: Metric--T MeasureTypeDate
birthdateMetric = fromJust $ {- castMetric =<< -} find (("birthdate" ==) . metricName) allMetrics

metricJSON :: JSON.ToObject o => Metric -> JSON.Record (Id Metric) o
metricJSON m@Metric{..} = JSON.Record metricId $
     "category" JSON..= categoryId metricCategory
  <> "name" JSON..= metricName
  <> "release" JSON..=? metricRelease
  <> "type" JSON..= show metricType
  <> "options" JSON..=? (if null metricOptions then empty else pure metricOptions)
  <> "assumed" JSON..=? metricAssumed
  <> "long" JSON..=? (True <? metricLong m)
  <> "description" JSON..=? metricDescription
  <> "required" JSON..=? metricRequired

{- schema synchronization:
20160201-nih_race
20160202-context_language
20160303-pregnancy_term
-}
