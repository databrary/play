{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell, TypeFamilies #-}
module Model.Funding.Types
  ( Funder(..)
  , Funding(..)
  ) where

import Data.Int (Int64)
import qualified Data.Text as T

import Model.Kind
import Model.Id.Types

type instance IdType Funder = Int64

data Funder = Funder
  { funderId :: Id Funder
  , funderName :: T.Text
  }

instance Kinded Funder where
  kindOf _ = "funder"

data Funding = Funding
  { fundingFunder :: Funder
  , fundingAwards :: [T.Text]
  }
