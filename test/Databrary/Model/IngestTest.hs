{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.IngestTest where

import qualified Data.HashMap.Strict as HMP
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Ingest

tests :: TestTree
tests = testGroup "Databrary.Model.Ingest"
    [ testCase "tbd"
        (True @?= True)
    ]
