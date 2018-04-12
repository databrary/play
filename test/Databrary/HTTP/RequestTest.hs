{-# LANGUAGE OverloadedStrings #-}
module Databrary.HTTP.RequestTest where
import Test.Tasty
import Test.Tasty.HUnit
-- import Data.Time (fromGregorian)
-- import Data.ByteString.Builder (toLazyByteString)
-- import Data.Attoparsec.ByteString (parseOnly)
import qualified Network.Wai as WAI
import qualified Network.HTTP.Types.Header as HTTP

import Databrary.HTTP.Request

test_all :: TestTree
test_all = testGroup "all"
  [ testCase "boolParameterValue-1"
      (boolParameterValue (Just "0") @?= False)
  , testCase "boolParameterValue-2"
      (boolParameterValue (Just "x") @?= True)
  , testCase "boolParameterValue-2"
      (boolParameterValue Nothing @?= True)
  , testCase "requestHost-1"
      (requestHost (WAI.defaultRequest) @?= "http://databrary.org")
  , testCase "requestHost-2"
      (requestHost (WAI.defaultRequest {WAI.requestHeaderHost = Just "host.com"} ) @?= "http://host.com")
  , testCase "lookupRequestHeader-1"
      (lookupRequestHeader "HDR_X" WAI.defaultRequest @?= Nothing)
  , testCase "lookupRequestHeader-2"
      (lookupRequestHeader HTTP.hAccept WAI.defaultRequest { WAI.requestHeaders = [(HTTP.hAccept, "text/plain")] }
         @?= Just "text/plain")
  , testCase "lookupRequestHeaders-1"
      (lookupRequestHeaders HTTP.hAccept WAI.defaultRequest @?= [])
  , testCase "lookupQueryParameters-1"
      (lookupQueryParameters "param1" WAI.defaultRequest @?= [])
  , testCase "lookupQueryParameters-2"
      (lookupQueryParameters
         "param1"
         WAI.defaultRequest { WAI.queryString = [("param1",Just "val1")] }
         @?= [Just "val1"])
  , testCase "lookupQueryParameters-3"
      (lookupQueryParameters
         "param1"
         WAI.defaultRequest { WAI.queryString = [("param1",Nothing)] }
         @?= [Nothing])
  ]
