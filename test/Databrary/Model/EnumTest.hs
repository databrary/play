module Databrary.Model.EnumTest where

import Data.Aeson
import Data.Aeson.Types
-- import Data.Text
import Test.Tasty.HUnit

import Databrary.Model.Enum
import Databrary.Model.Permission

-- Session demonstrating how to use Enum functions
unit_Enum_examples :: Assertion
unit_Enum_examples = do
    parseEither parseJSONEnum (String "Public") @?= Right PermissionPUBLIC
