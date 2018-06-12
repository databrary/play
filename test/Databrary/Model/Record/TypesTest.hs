{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Record.TypesTest where

import qualified Data.ByteString as BS
import Data.Time (fromGregorian)
-- import Hedgehog
-- import qualified Hedgehog.Gen as Gen
-- import Test.Tasty

-- import Databrary.Model.Category
import Databrary.Model.Category.TypesTest
import Databrary.Model.Id.Types
-- import Databrary.Model.Metric
import Databrary.Model.Record.Types
-- import Databrary.Model.Volume.Types

participantRecordAll :: ParticipantRecord
participantRecordAll =
    ParticipantRecord
         { prdId = Field "1" "1"
         , prdInfo = Field "infoval" "infoval"
         , prdDescription = Field "descval" "descval"
         , prdBirthdate = Field "2011-06-17" (fromGregorian 2011 6 17)
         , prdGender = Field "Male" "Male"
         , prdRace = Field "White" "White"
         , prdEthnicity = Field "Hispanic or Latino" "Hispanic or Latino"
         , prdGestationalAge = Field "2.5" 2.5
         , prdPregnancyTerm = Field "Preterm" "Preterm"
         , prdBirthWeight = Field "10.5" 10.5
         , prdDisability = Field "normal" "normal"
         , prdLanguage = Field "English" "English"
         , prdCountry = Field "USA" "USA"
         , prdState = Field "MA" "MA"
         , prdSetting = Field "Lab" "Lab"
         }

participantRecordId :: BS.ByteString -> ParticipantRecord
participantRecordId idVal =
    emptyParticipantRecord { prdId = Field idVal idVal }

emptyParticipantRecord :: ParticipantRecord
emptyParticipantRecord =
    ParticipantRecord
         { prdId = FieldUnused
         , prdInfo = FieldUnused
         , prdDescription = FieldUnused
         , prdBirthdate = FieldUnused
         , prdGender = FieldUnused
         , prdRace = FieldUnused
         , prdEthnicity = FieldUnused
         , prdGestationalAge = FieldUnused
         , prdPregnancyTerm = FieldUnused
         , prdBirthWeight = FieldUnused
         , prdDisability = FieldUnused
         , prdLanguage = FieldUnused
         , prdCountry = FieldUnused
         , prdState = FieldUnused
         , prdSetting = FieldUnused
         }

testRecordRow1 :: RecordRow
testRecordRow1 =
    RecordRow {
        recordId = Id 100
      , recordCategory = testCategory1
    }
