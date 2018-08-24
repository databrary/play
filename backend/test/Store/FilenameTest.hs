{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Store.FilenameTest where

import Test.Tasty.HUnit

import Store.Filename

unit_makeFilename :: Assertion
unit_makeFilename =
    -- example
    makeFilename ["prefixiswaytoolongtofitintothispathsoitwillbecutoff", "file.hs"]
        @?= "prefixiswaytoolongtofitintothisp-file.hs"
