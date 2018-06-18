{-# LANGUAGE OverloadedStrings #-}
module Databrary.Model.Paginate
  ( Paginate(..)
  , def
  ) where

import Data.Default.Class (Default(..))
import Data.Int (Int32)

-- | Denote information for retrieving one page, most likely from a query result
data Paginate = Paginate
  { paginateOffset :: !Int32 -- ^ number of rows to skip before starting
  , paginateLimit :: !Int32 -- ^ max number of rows to retrieve after offset
  } -- deriving (Eq, Ord, Show)

-- | A page must have at least one row requested, and can only request up to 129 rows in one page
instance Bounded Paginate where
  minBound = Paginate 0 1
  maxBound = Paginate maxBound 129

-- | Default paging starts from the beginning and retrieves up to 25 rows
instance Default Paginate where
  def = Paginate 0 25

-- | ways to traverse forward and backwards between pages, using limit
instance Enum Paginate where
  succ (Paginate o l) = Paginate (o+l) l
  pred (Paginate 0 _) = error "pred Paginate: invalid argument"
  pred (Paginate o l) = Paginate (o-l `max` 0) l
  -- | toEnum seems to map to intuitive concept of a page
  toEnum i = Paginate (d*fromIntegral i) d where d = paginateLimit def
  -- | fromEnum seems to map to an individual row, instead of a page
  fromEnum (Paginate o l) = fromIntegral $ o + l - 1 `div` l
