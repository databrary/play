{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Funding.SQL
  ( selectVolumeFunding
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Databrary.Model.SQL.Select
import Databrary.Model.Funding.Types

makeFunding :: [Maybe T.Text] -> Funder -> Funding
makeFunding a f = Funding f (map (fromMaybe (error "NULL funding.award")) a)

selectVolumeFunding :: Selector -- ^ @'Funding'@
selectVolumeFunding = selectJoin '($)
  [ selectColumns 'makeFunding "volume_funding" ["awards"]
  , joinOn "volume_funding.funder = funder.fundref_id" (selectColumns 'Funder "funder" ["fundref_id", "name"])
  ]
