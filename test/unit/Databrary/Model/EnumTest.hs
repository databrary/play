module Databrary.Model.EnumTest
   ( tests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Enum
import Databrary.Model.Kind
-- import Database.PostgreSQL.Typed.Enum (PGEnum)
import Databrary.Model.Release.Types

{-
data SomeType = SomeVal

instance Kinded SomeType where
  kindOf _ = "SomeType"

instance DBEnum SomeType

instance PGEnum SomeType
-- makePGEnum "" "" ("SomeType_" ++)
-}

tests :: TestTree
tests = testGroup "Databrary.Model.Enum"
  [ testCase "readDBEnum-1"
      (readDBEnum "Public" @?= Just ReleasePUBLIC)
  , testCase "readDBEnum-2"
      (readDBEnum "public" @?= Just ReleasePUBLIC)
  , testCase "readDBEnum-3"
      (readDBEnum "invalid" @?= (Nothing :: Maybe Release))
  ]
