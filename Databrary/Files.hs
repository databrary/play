module Databrary.Files
  ( RawFilePath
  , (</>)
  , catchDoesNotExist
  , modificationTimestamp
  , fileInfo
  , setFileTimestamps
  , removeFile
  , createDir
  , compareFiles
  , rawFilePath
  , unRawFilePath
  ) where

import Control.Arrow ((&&&))
import Control.Exception (handleJust)
import Control.Monad (guard, liftM2)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Maybe (isJust)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import System.Posix.ByteString.FilePath (RawFilePath)
import System.Posix.FilePath ((</>))
import qualified System.Posix as P
import qualified System.Posix.ByteString as RP
import System.Posix.Types (FileMode)
import System.IO (withBinaryFile, IOMode(ReadMode), hGetBufSome)
import System.Posix.Types (FileOffset)
import System.IO.Error (isDoesNotExistError, isAlreadyExistsError)

import Databrary.Ops
import Databrary.Model.Time

unRawFilePath :: RawFilePath -> IO FilePath
unRawFilePath b = do
  enc <- getFileSystemEncoding
  BS.useAsCStringLen b $ GHC.peekCStringLen enc

rawFilePath :: FilePath -> IO RawFilePath
rawFilePath s = do
  enc <- getFileSystemEncoding
  GHC.withCStringLen enc s BS.packCStringLen

catchOnlyIO :: (IOError -> Bool) -> IO a -> IO (Maybe a)
catchOnlyIO c f = handleJust (guard . c) (\_ -> return Nothing) $ Just <$> f

catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist = catchOnlyIO isDoesNotExistError

catchAlreadyExists :: IO a -> IO (Maybe a)
catchAlreadyExists = catchOnlyIO isAlreadyExistsError

modificationTimestamp :: P.FileStatus -> Timestamp
modificationTimestamp = posixSecondsToUTCTime . P.modificationTimeHiRes

fileInfo :: RawFilePath -> IO (Maybe (FileOffset, Timestamp))
fileInfo f =
  (=<<) (liftM2 (?>) P.isRegularFile $ P.fileSize &&& modificationTimestamp)
  <$> catchDoesNotExist (RP.getFileStatus f)

setFileTimestamps :: RawFilePath -> Timestamp -> Timestamp -> IO ()
setFileTimestamps f a m = RP.setFileTimesHiRes f (utcTimeToPOSIXSeconds a) (utcTimeToPOSIXSeconds m)

removeFile :: RawFilePath -> IO Bool
removeFile f = isJust <$> catchDoesNotExist (RP.removeLink f)

createDir :: RawFilePath -> FileMode -> IO Bool
createDir f m = isJust <$> catchAlreadyExists (RP.createDirectory f m)

-- | Returns 'True' if files are identical
compareFiles :: RawFilePath -> RawFilePath -> IO Bool
compareFiles f1 f2 = do
  s1 <- RP.getFileStatus f1
  s2 <- RP.getFileStatus f2
  f1p <- unRawFilePath f1
  f2p <- unRawFilePath f2
  if P.deviceID s1 == P.deviceID s2 && P.fileID s1 == P.fileID s2 then return True
    else if P.fileSize s1 /= P.fileSize s2 then return False
    else withBinaryFile f1p ReadMode $ withBinaryFile f2p ReadMode . cmp where
    cmp h1 h2 = do
      b1 <- BS.hGet h1 defaultChunkSize
      b2 <- BS.hGet h2 defaultChunkSize
      if b1 == b2
        then if BS.null b1 then return True else cmp h1 h2
        else return False

