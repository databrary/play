{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Party.TypesTest where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Permission.Types
import Databrary.Model.Party.Types

-- accessCommunity :: Access -- correct?
-- accessCommunity = Access { accessSite' = PermissionSHARED, accessMember' = PermissionSHARED }

tests :: TestTree
tests = testGroup "Databrary.Model.Permission.Types"
    [
    ]
