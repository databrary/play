{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.RecordTest (tests) where

import qualified Data.HashMap.Strict as HMP
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Record

tests :: TestTree
tests = testGroup "Databrary.Model.Record"
    [ testCase "extractParticipantFieldRows"
        (extractParticipantFieldRows ["c1"] (V.fromList [(HMP.fromList [("c1", "val1")])]) @?= [("c1", ["val1"])])
    ]
