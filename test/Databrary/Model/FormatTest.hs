{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.FormatTest where

-- import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Format

unit_mimeTypeTop_example :: Assertion
unit_mimeTypeTop_example = do
    mimeTypeTop "text/plain" @?= "text"
