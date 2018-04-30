{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Store.FilenameTest where

import Test.Tasty.HUnit

import Databrary.Store.Filename

unit_makeFilename :: Assertion
unit_makeFilename =
    -- example
    makeFilename ["prefixiswaytoolongtofitintothispathsoitwillbecutoff", "file.hs"]
        @?= "prefixiswaytoolongtofitintothisp-file.hs"
