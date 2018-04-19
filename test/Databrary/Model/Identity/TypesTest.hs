{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Identity.TypesTest where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Identity.Types

test_foldIdentity :: TestTree
test_foldIdentity = testGroup "foldIdentity"
    [ testCase "example" 
          (runFoldIdentity NotIdentified @?= Default)
    , testCase "typical" (do
          runFoldIdentity PreIdentified @?= Default
          runFoldIdentity (ReIdentified undefined) @?= Default
          runFoldIdentity (Identified undefined) @?= Extracted)
    ]

runFoldIdentity :: Identity -> Result
runFoldIdentity = foldIdentity Default (const Extracted)

data Result = Default | Extracted deriving (Eq, Show)

test_identitySuperuser :: [TestTree]
test_identitySuperuser = 
    [ testCase "typical" (do
          identitySuperuser (ReIdentified undefined) @? "reidentified is superuser")
          -- why True? is this because transcoding needs higher privileges to update asset?
    ]
