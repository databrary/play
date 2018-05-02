{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.PaginateTest where

import Hedgehog
import Hedgehog.Gen as Gen
-- import Hedgehog.Range as Range
import Test.Tasty.HUnit

import Databrary.Model.Paginate

unit_paginate_succ :: Assertion
unit_paginate_succ = do
    -- example
    succ (Paginate 1 20) @?= Paginate 21 20

unit_paginate_pred :: Assertion
unit_paginate_pred = do
    -- example
    pred (Paginate 100 25) @?= Paginate 75 25

unit_paginate_toEnum :: Assertion
unit_paginate_toEnum = do
    -- example
    toEnum 3 @?= Paginate 75 25

unit_paginate_fromEnum :: Assertion
unit_paginate_fromEnum = do
    -- example
    fromEnum (Paginate 75 25) @?= 100

genPaginate :: Gen Paginate
genPaginate = Gen.enumBounded
