{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Funding.Types
  ( Funder(..)
  , Funding(..)
  , makeFunding
  ) where

import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

-- import Databrary.Has (Has(..))
import Databrary.Model.Kind
import Databrary.Model.Id.Types

type instance IdType Funder = Int64

data Funder = Funder
  { funderId :: Id Funder
  , funderName :: T.Text
  } deriving (Eq, Show)

-- makeHasRec ''Funder ['funderId]
-- instance Has (Id Funder) Funder where
--   view = funderId

instance Kinded Funder where
  kindOf _ = "funder"

data Funding = Funding
  { fundingFunder :: Funder
  , fundingAwards :: [T.Text]
  } deriving (Eq, Show)

-- makeHasRec ''Funding ['fundingFunder]
-- instance Has Funder Funding where
--   view = fundingFunder
-- instance Has (Id Funder) Funding where
--   view = (view . fundingFunder)

makeFunding :: [Maybe T.Text] -> Funder -> Funding
makeFunding a f = Funding f (map (fromMaybe (error "NULL funding.award")) a)
