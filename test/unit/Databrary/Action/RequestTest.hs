{-# LANGUAGE OverloadedStrings #-}
module Databrary.Action.RequestTest
   ( tests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Action.Request
import qualified Network.Wai as WAI

tests :: TestTree
tests = testGroup "Databrary.Action.Request"
  [ testCase "isDatabraryClient-1"
      (isDatabraryClient WAI.defaultRequest @?= False)
  , testCase "isDatabraryClient-2"
      (isDatabraryClient WAI.defaultRequest { WAI.requestHeaders = [("x-requested-with", "DatabraryClient")] } 
         @?= True)
  ]
