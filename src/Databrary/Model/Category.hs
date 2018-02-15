{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Model.Category
  ( module Databrary.Model.Category.Types
  , allCategories
  , getCategory
  , getCategory'
  , categoryJSON
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.Monoid ((<>))
import qualified Data.Text

import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Category.Types

allCategories :: [Category]
allCategories =  -- TODO: get rid of Category table, only present here instead
    [Category
       (Id 1)
       (Data.Text.pack "participant")
       (Just
          (Data.Text.pack
             "An individual human subject whose data are used or represented")),
     Category
       (Id 2)
       (Data.Text.pack "pilot")
       (Just
          (Data.Text.pack
             "Indicates that the methods used were not finalized or were non-standard")),
     Category
       (Id 3)
       (Data.Text.pack "exclusion")
       (Just (Data.Text.pack "Indicates that data were not usable")),
     Category
       (Id 4)
       (Data.Text.pack "condition")
       (Just
          (Data.Text.pack
             "An experimenter-determined manipulation (within or between sessions)")),
     Category
       (Id 5)
       (Data.Text.pack "group")
       (Just
          (Data.Text.pack
             "A grouping determined by an aspect of the data (participant ability, age, grade level, experience, longitudinal visit, measurements used/available)")),
     Category
       (Id 6)
       (Data.Text.pack "task")
       (Just
          (Data.Text.pack
             "A particular task, activity, or phase of the session or study")),
     Category
       (Id 7)
       (Data.Text.pack "context")
       (Just
          (Data.Text.pack
             "A particular setting or other aspect of where/when/how data were collected"))]

categoriesById :: IntMap.IntMap Category
categoriesById = IntMap.fromAscList $ map (\a -> (fromIntegral $ unId $ categoryId a, a)) allCategories

getCategory :: Id Category -> Maybe Category
getCategory (Id i) = IntMap.lookup (fromIntegral i) categoriesById

getCategory' :: Id Category -> Category
getCategory' (Id i) = categoriesById IntMap.! fromIntegral i

categoryJSON :: JSON.ToObject o => Category -> JSON.Record (Id Category) o
categoryJSON Category{..} = JSON.Record categoryId $
     "name" JSON..= categoryName
  <> "description" JSON..=? categoryDescription
