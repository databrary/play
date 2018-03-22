{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Record.TypesTest where

import qualified Data.ByteString as BS
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Record.Types

participantRecordAll :: ParticipantRecord
participantRecordAll =
    ParticipantRecord
         { prdId = Just "1"
         , prdInfo = Just "infoval"
         , prdDescription = Just "descval"
         , prdBirthdate = Just "2011-06-17"
         , prdGender = Just "Male"
         , prdRace = Just "White"
         , prdEthnicity = Just "Hispanic or Latino"
         , prdGestationalAge = Just "2.5"
         , prdPregnancyTerm = Just "Preterm"
         , prdBirthWeight = Just "10.5"
         , prdDisability = Just "normal"
         , prdLanguage = Just "English"
         , prdCountry = Just "USA"
         , prdState = Just "MA"
         , prdSetting = Just "Lab"
         }

participantRecordId :: BS.ByteString -> ParticipantRecord
participantRecordId idVal =
    emptyParticipantRecord { prdId = Just idVal }

emptyParticipantRecord :: ParticipantRecord
emptyParticipantRecord =
    ParticipantRecord
         { prdId = Nothing
         , prdInfo = Nothing
         , prdDescription = Nothing
         , prdBirthdate = Nothing
         , prdGender = Nothing
         , prdRace = Nothing
         , prdEthnicity = Nothing
         , prdGestationalAge = Nothing
         , prdPregnancyTerm = Nothing
         , prdBirthWeight = Nothing
         , prdDisability = Nothing
         , prdLanguage = Nothing
         , prdCountry = Nothing
         , prdState = Nothing
         , prdSetting = Nothing
         }

tests :: TestTree
tests = testGroup "Databrary.Model.Record.Types"
    [
    ]
