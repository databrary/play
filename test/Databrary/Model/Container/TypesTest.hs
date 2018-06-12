{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Container.TypesTest where

-- import Data.Text (Text)
-- import Data.Time
-- import Hedgehog
-- import Hedgehog.Gen as Gen
-- import Hedgehog.Range as Range
import Test.Tasty.HUnit

import Databrary.Model.Container
-- import Databrary.Model.Permission.Types
-- import Databrary.Model.Party.Types
-- import Databrary.Model.Party.TypesTest
import Databrary.Model.Release.Types
-- import Databrary.Model.Id.Types
-- import Databrary.Model.Volume.Types
-- import Databrary.Model.Volume.TypesTest

unit_getContainerRelease :: Assertion
unit_getContainerRelease =
  -- example
  (effRelPrivate . getContainerRelease) (blankContainer undefined) @?= ReleasePRIVATE
  -- typical
  -- edge cases
