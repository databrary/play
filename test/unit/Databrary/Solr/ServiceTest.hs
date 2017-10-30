{-# LANGUAGE OverloadedStrings #-}
module Databrary.Solr.ServiceTest
   ( tests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Solr.Service

tests :: TestTree
tests = testGroup "Databrary.Solr.Service"
  [ testCase "confSolr-1"
       (True @?= True)
  ]

