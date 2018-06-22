{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Model.Age
  ( Age(..)
  , age
  , yearsAge
  , ageTime
  , ageLimit
  ) where

import Data.Time (diffDays, DiffTime, secondsToDiffTime)

import qualified JSON as JSON
import Model.Time

newtype Age =
  Age {
      ageDays :: Int -- ^ Totals days constituting age. E.g. 1.5 years old = 548 days
    } deriving (Eq, Ord) -- (Num, Show)

instance JSON.ToJSON Age where
  toJSON (Age days) = JSON.Number $ fromIntegral days

-- | subtract second date (usually current date) from first date (birthdate), convert difference into age
age :: Date -> Date -> Age
age b d = Age $ fromInteger $ diffDays d b

-- | convert a fractional value of years into a count of days, building an Age object with that value
yearsAge :: Real a => a -> Age
yearsAge y = Age $ ceiling $ (365.24219 :: Double) * realToFrac y

-- | convert total days spanning an Age value into the equivalent number of seconds (as a time interval)
ageTime :: Age -> DiffTime
ageTime (Age n) = secondsToDiffTime $ 86400 * fromIntegral n

-- | upper bound for allowed age values, intended for validation
ageLimit :: Age
ageLimit = yearsAge (90 :: Double)
