{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Record.TypesTest where

import qualified Data.ByteString as BS
import Data.Time (fromGregorian)
import Test.Tasty

import Databrary.Model.Record.Types

participantRecordAll :: ParticipantRecord
participantRecordAll =
    ParticipantRecord
         { prdId = Just (Just ("1", "1"))
         , prdInfo = Just (Just ("infoval", "infoval"))
         , prdDescription = Just (Just ("descval", "descval"))
         , prdBirthdate = Just (Just (fromGregorian 2011 6 17, "2011-06-17"))
         , prdGender = Just (Just ("Male", "Male"))
         , prdRace = Just (Just ("White", "White"))
         , prdEthnicity = Just (Just ("Hispanic or Latino", "Hispanic or Latino"))
         , prdGestationalAge = Just (Just (2.5, "2.5"))
         , prdPregnancyTerm = Just (Just ("Preterm", "Preterm"))
         , prdBirthWeight = Just (Just (10.5, "10.5"))
         , prdDisability = Just (Just ("normal", "normal"))
         , prdLanguage = Just (Just ("English", "English"))
         , prdCountry = Just (Just ("USA", "USA"))
         , prdState = Just (Just ("MA", "MA"))
         , prdSetting = Just (Just ("Lab", "Lab"))
         }

participantRecordId :: BS.ByteString -> ParticipantRecord
participantRecordId idVal =
    emptyParticipantRecord { prdId = Just (Just (idVal, idVal)) }

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

test_all :: [TestTree]
test_all =
    [
    ]
