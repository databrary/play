{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module FilesTest where

import Control.Exception (finally)
import Crypto.Hash (SHA1, Digest)
import Data.ByteString.Char8 (unpack)
import System.Posix.ByteString (ownerWriteMode, removeDirectory)
import Test.Tasty.HUnit

import Files

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
unit_createDir =
    finally
        (do -- example
            created1 <- createDir "/tmp/testcreate" ownerWriteMode
            created2 <- createDir "/tmp/testcreate" ownerWriteMode
            not created1 || (created1 && not created2) @? "Can't create same directory twice")
        (removeDirectory "/tmp/testcreate")

unit_compareFiles :: Assertion
unit_compareFiles =
    let file1 = "/tmp/comparefilestest"
        file2 = "/tmp/comparefilestest2"
    in finally
        (do -- example
            writeFile (unpack file1) "abc"
            writeFile (unpack file2) "abc"
            res <- compareFiles file1 file2
            res @? "same contents should be same")
        (mapM_ removeFile [file1, file2])

unit_hashFiles :: Assertion
unit_hashFiles = finally
    (do -- example
        writeFile "/tmp/hashFilesTest" "efg"
        (dig :: Digest SHA1) <- hashFile "/tmp/hashFilesTest"
        show dig @?= "cbf019b764b9477080c5a9a748a2911a5fa6d614")
    (removeFile "/tmp/hashFilesTest")
