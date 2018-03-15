{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Controller.IngestTest where

import qualified Data.HashMap.Strict as HMP
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Controller.Ingest

tests :: TestTree
tests = testGroup "Databrary.Controller.Ingest"
    [ testCase "tbd"
        (True @?= True)
    ]
