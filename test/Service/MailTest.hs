{-# LANGUAGE OverloadedStrings #-}
module Service.MailTest
where

import Test.Tasty
import Test.Tasty.HUnit

import Service.Mail

test_all :: [TestTree]
test_all =
  [ testCase "wrapText-1"
       (wrapText 10 "two lines of stuff" @?= "two lines\nof stuff\n")
  ]
