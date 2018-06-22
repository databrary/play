{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module HasTest where

import Control.Monad.Reader
import Test.Tasty.HUnit

import Has

unit_view :: Assertion
unit_view =
  -- example
  view (Rec1 { recField1 = True }) @?= True

unit_peek :: Assertion
unit_peek = do
  -- example
  field1Val <- runReaderT (peek) (Rec1 { recField1 = True })
  field1Val @?= True

unit_peeks :: Assertion
unit_peeks = do
  -- example
  notField1Val <- runReaderT (peeks not) (Rec1 { recField1 = True })
  notField1Val @?= False

data Rec1 =
    Rec1 {
          recField1 :: Bool
    } deriving (Show, Eq)

instance Has Bool Rec1 where
    view = recField1
