module Databrary.FilesTest where

import System.Posix.Files.ByteString
import Test.Tasty.HUnit

import Databrary.Files

unit_rawFilePath :: Assertion
unit_rawFilePath = do
    -- example
    res <- rawFilePath "/a/b"
    res @?= "/a/b"

unit_removeFile :: Assertion
unit_removeFile = do
    -- example
    removed <- removeFile "/tmp/doesnotexist"
    removed @?= False

unit_createDir :: Assertion
unit_createDir = do
    -- example
    created1 <- createDir "/tmp/testcreate" ownerWriteMode
    created2 <- createDir "/tmp/testcreate" ownerWriteMode
    not created1 || (created1 && not created2) @? "Can't create same directory twice"
