{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.MailTest
where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Service.Mail

test_all :: [TestTree]
test_all =
  [ testCase "wrapText-1"
       (wrapText 10 "two lines of stuff" @?= "two lines\nof stuff\n")
  ]
