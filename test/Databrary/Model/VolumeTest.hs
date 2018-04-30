{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving
   , TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
module Databrary.Model.VolumeTest where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure

import TestHarness

import Databrary.Model.Identity
import Databrary.Model.Volume
import Databrary.Service.DB

unit_getVolumeAlias :: Assertion
unit_getVolumeAlias =
    -- example
    getVolumeAlias blankVolume @?= Nothing
    -- typical
    -- edge cases

test_findVolumes :: TestTree
test_findVolumes = ignoreTest -- Because "??"
    (testCase "" _unit_findVolumes)

_unit_findVolumes :: Assertion
_unit_findVolumes = do
    let ident = PreIdentified
    cn <- loadPGDatabase >>= pgConnect
    let ctxt = TestContext { ctxConn = cn, ctxIdentity = ident }
    vs <- runReaderT (findVolumes volumeFilter1) ctxt
    length vs @?= 2

volumeFilter1 :: VolumeFilter
volumeFilter1 =
    mempty
