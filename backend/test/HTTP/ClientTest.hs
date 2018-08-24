{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module HTTP.ClientTest where

import qualified Network.HTTP.Client as HC
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

import Data.Aeson
import Data.Aeson.Types
import HTTP.Client

test_httpRequestJSON :: TestTree
test_httpRequestJSON =
    -- don't need to run this every time, enable when desired
    ignoreTest (testCase "httpRequestJSON" _unit_httpRequestJSON)

_unit_httpRequestJSON :: Assertion
_unit_httpRequestJSON = do
    -- example
    hc <- initHTTPClient
    req <- HC.parseRequest "http://httpbin.org/get"
    Just val <- httpRequestJSON req hc
    let eVal = parseEither (withObject "req" (\o -> o .: "url")) val
    eVal @?= (Right "http://httpbin.org/get" :: Either String String)
