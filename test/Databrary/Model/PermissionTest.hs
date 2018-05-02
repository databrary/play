{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.PermissionTest where

-- import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Identity.Types
import Databrary.Model.Permission
import TestHarness

unit_readRelease_example :: Assertion
unit_readRelease_example = do
    readRelease PermissionNONE @?= Nothing

-- working up towards eventual pure tests of checkMemberAdmin
unit_accessMember :: Assertion
unit_accessMember = do
    -- example
    accessMember (TestContext { ctxIdentity = NotLoggedIn }) @?= PermissionNONE
    -- typical
    -- accessMember (TestContext { ctxIdentity = mkSession ... }) @?= PermissionNONE
    -- edge case
    accessMember (TestContext { ctxIdentity = IdentityNotNeeded }) @?= PermissionNONE
