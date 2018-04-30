{-# LANGUAGE ScopedTypeVariables #-}
module Databrary.FilesTest where

import Crypto.Hash (SHA1, Digest)
-- import System.IO
import System.Posix.Files.ByteString
import Test.Tasty.HUnit

import Databrary.Files

unit_rawFilePath :: Assertion
unit_rawFilePath = do
    -- example
    res <- rawFilePath "/a/b"
    res @?= "/a/b"

unit_unRawFilePath :: Assertion
unit_unRawFilePath = do
    -- example
    res <- rawFilePath "/a/b"
    unres <- unRawFilePath res
    unres @?= "/a/b"

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

unit_compareFiles :: Assertion
unit_compareFiles = do
    -- example
    writeFile "/tmp/comparefilestest" "abc"
    writeFile "/tmp/comparefilestest2" "abc"
    res <- compareFiles "/tmp/comparefilestest" "/tmp/comparefilestest2"
    res @? "same contents should be same"

unit_hashFiles :: Assertion
unit_hashFiles = do
    -- example
    writeFile "/tmp/hashFilesTest" "efg"
    (dig :: Digest SHA1) <- hashFile "/tmp/hashFilesTest"
    show dig @?= "cbf019b764b9477080c5a9a748a2911a5fa6d614"
