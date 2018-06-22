{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Model.Category.TypesTest where

-- import Test.Tasty.HUnit

import Model.Category.Types
import Model.Id.Types

testCategory1 :: Category
testCategory1 =
    Category {
          categoryId = Id (-500)
        , categoryName = "participant"
        , categoryDescription = Nothing -- where is this loaded from??
    }
