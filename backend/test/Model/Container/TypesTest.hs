{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Model.Container.TypesTest where

-- import Data.Text (Text)
-- import Data.Time
-- import Hedgehog
-- import Hedgehog.Gen as Gen
-- import Hedgehog.Range as Range
import Test.Tasty.HUnit

import Model.Container
-- import Model.Permission.Types
-- import Model.Party.Types
-- import Model.Party.TypesTest
import Model.Release.Types
-- import Model.Id.Types
-- import Model.Volume.Types
-- import Model.Volume.TypesTest

unit_getContainerRelease :: Assertion
unit_getContainerRelease =
  -- example
  (effRelPrivate . getContainerRelease) (blankContainer undefined) @?= ReleasePRIVATE
  -- typical
  -- edge cases
