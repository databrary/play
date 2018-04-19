{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Identity.TypesTest where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Identity.Types

test_foldIdentity :: TestTree
test_foldIdentity = testGroup "foldIdentity"
    [ testCase "example" 
          (runFoldIdentity NotIdentified @?= True)
    , testCase "typical" (do
          runFoldIdentity PreIdentified @?= True
          runFoldIdentity (ReIdentified undefined) @?= True
          runFoldIdentity (Identified undefined) @?= False)
    ]

runFoldIdentity :: Identity -> Bool
runFoldIdentity = foldIdentity True (const False)

test_identitySuperuser :: [TestTree]
test_identitySuperuser = 
    [ testCase "typical" (do
          identitySuperuser (ReIdentified undefined) @?= True)
          -- why True? is this because transcoding needs higher privileges to update asset?
    ]
