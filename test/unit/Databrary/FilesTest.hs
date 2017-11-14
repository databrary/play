module Databrary.FilesTest
   ( tests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Files
import Databrary.Store.Filename
import Databrary.Store.Asset
import Databrary.Store.Types
import Databrary.Store.Stage
import Databrary.Store.Temp
import Databrary.Store.Upload
import Databrary.Store.Zip

tests :: TestTree
tests = testGroup "Databrary.Files"
  [ testCase "rawFilePath-1"
       (True @?= True)
  ]

