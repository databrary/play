module Model.PermissionTest where

import Test.Tasty
import Test.Tasty.HUnit

import Model.Permission
import TestHarness ()

test_checkPermission :: [TestTree]
test_checkPermission =
    [ testCase "Can read when granted edit"
        $ checkPermission (const PermissionEDIT) () PermissionREAD
        @?= PermissionGranted ()
    , testCase "Can edit when granted admin"
        $ checkPermission (const PermissionADMIN) () PermissionEDIT
        @?= PermissionGranted ()
    , testCase "Cannot edit when granted read"
        $ checkPermission (const PermissionREAD) () PermissionEDIT
        @?= PermissionDenied
    , testCase "Cannot read when granted 'public'"
        $ checkPermission (const PermissionPUBLIC) () PermissionREAD
        @?= PermissionDenied
    ]
