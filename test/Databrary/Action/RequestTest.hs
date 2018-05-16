{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Action.RequestTest where

import Network.Wai
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Action.Request

test_isDatabraryClient :: [TestTree]
test_isDatabraryClient =
    [ testCase "example" $ isDatabraryClient defaultRequest @?= False
    , testCase "typical use"
        $ isDatabraryClient
            (defaultRequest
                { requestHeaders = [("x-requested-with", "DatabraryClient")]
                }
            )
        @?= True
    ]
