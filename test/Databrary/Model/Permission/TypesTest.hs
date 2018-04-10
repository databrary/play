{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Permission.TypesTest where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Permission.Types

{- PermissionNONE | PermissionPUBLIC | PermissionSHARED | PermissionREAD | PermissionEDIT |
   PermissionADMIN -}

accessCommunity :: Access -- correct?
accessCommunity = Access { accessSite' = PermissionSHARED, accessMember' = PermissionSHARED }

accessPublic :: Access -- correct?
accessPublic = Access { accessSite' = PermissionPUBLIC, accessMember' = PermissionPUBLIC }

accessNone :: Access -- correct?
accessNone = Access { accessSite' = PermissionNONE, accessMember' = PermissionNONE }

test_all :: TestTree
test_all = testGroup "all"
    [
    ]
