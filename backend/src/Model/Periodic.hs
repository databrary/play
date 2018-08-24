module Model.Periodic
  ( Period(..)
  ) where

import Control.Exception (Exception(..), asyncExceptionToException, asyncExceptionFromException)

data Period
  = PeriodDaily
  | PeriodWeekly
  deriving (Eq, Ord, Show)

instance Exception Period where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

