{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Category.TypesTest where

-- import Test.Tasty.HUnit

import Databrary.Model.Category.Types
import Databrary.Model.Id.Types

testCategory1 :: Category
testCategory1 =
    Category {
          categoryId = Id (-500)
        , categoryName = "participant"
        , categoryDescription = Nothing -- where is this loaded from??
    }
