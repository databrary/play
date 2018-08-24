{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Model.Category.Types
  ( Category(..)
  ) where

import Data.Function (on)
import Data.Int (Int16)
import qualified Data.Text as T
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLift)

import Model.Kind
import Model.Id.Types

type instance IdType Category = Int16

data Category = Category
  { categoryId :: !(Id Category)
  , categoryName :: !T.Text
  , categoryDescription :: !(Maybe T.Text)
  } deriving (Show)

instance Kinded Category where
  kindOf _ = "category"

instance Eq Category where
  (==) = on (==) categoryId
  (/=) = on (/=) categoryId

deriveLift ''Category
