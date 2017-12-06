module Databrary.FilesHash
  ( hashFile
  ) where

import Crypto.Hash (HashAlgorithm, hashInit, hashUpdate, hashFinalize, Digest)
import Data.ByteArray (MemView(..))
import qualified Data.ByteString as BS
import Foreign.Marshal.Alloc (allocaBytes)
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import System.IO (withBinaryFile, IOMode(ReadMode), hGetBufSome)
import System.Posix.ByteString.FilePath (RawFilePath)

unRawFilePath :: RawFilePath -> IO FilePath --duplicated from Files
unRawFilePath b = do
  enc <- getFileSystemEncoding
  BS.useAsCStringLen b $ GHC.peekCStringLen enc

hashFile :: (HashAlgorithm a) => RawFilePath -> IO (Digest a)
hashFile f = do
  f' <- unRawFilePath f
  withBinaryFile f' ReadMode $ \h ->
    allocaBytes z $ \b ->
      run h b hashInit where
  run h b s = do
    n <- hGetBufSome h b z
    if n == 0
      then return $! hashFinalize s
      else run h b $! hashUpdate s (MemView b n)
  z = 32786
