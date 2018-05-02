module Databrary.Model.Kind
  ( Kinded(..)
  ) where

import Data.String (IsString)

-- | Types with a self-describing short name for their type
class Kinded a where
  kindOf :: IsString s => a -> s
