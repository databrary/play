{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Model.RecordTest where

import qualified Data.HashMap.Strict as HMP
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Model.Record

test_all :: [TestTree]
test_all =
    [ testCase "extractParticipantFieldRows"
        (extractParticipantFieldRows ["c1"] (V.fromList [(HMP.fromList [("c1", "val1")])]) @?= [("c1", ["val1"])])
    ]
