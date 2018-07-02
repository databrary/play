{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell, TypeFamilies #-}
module Model.Funding.Types
  ( Funder(..)
  , Funding(..)
  , makeFunding
  ) where

import Data.Int (Int64)
import Data.Maybe (fromMaybe)
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

makeFunding :: [Maybe T.Text] -> Funder -> Funding
makeFunding a f = Funding f (map (fromMaybe (error "NULL funding.award")) a)
