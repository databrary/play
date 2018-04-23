{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.PaginateTest where

-- import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Paginate

unit_paginate_succ_example :: Assertion
unit_paginate_succ_example = do
    succ (Paginate 1 20) @?= Paginate 21 20
