{-# LANGUAGE OverloadedStrings #-}
module Controller.Upload
  ( uploadStart
  , uploadChunk
  , testChunk
  -- * for testing
  , createUploadSetSize
  , UploadStartRequest(..)
  , writeChunk
  ) where

import Control.Exception (bracket)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Word (Word64)
import Foreign.C.Types (CSize(..))
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (castPtr)
import Network.HTTP.Types (ok200, noContent204, badRequest400)
import qualified Network.Wai as Wai
import System.IO (SeekMode(AbsoluteSeek))
import System.Posix.FilePath (RawFilePath)
import System.Posix.Files.ByteString (setFdSize)
import System.Posix.IO.ByteString (openFd, OpenMode(ReadOnly, WriteOnly), defaultFileFlags, exclusive, closeFd, fdSeek, fdWriteBuf, fdReadBuf)
import System.Posix.Types (COff(..))

import Has (view, peek, peeks, focusIO, MonadHas)
import qualified JSON
import Service.DB (MonadDB)
import Service.Entropy (Entropy)
import Service.Log
import Model.Id
import Model.Identity (MonadHasIdentity)
import Model.Permission
import Model.Volume hiding (getVolume)
import Model.Format
import Model.Token
import Store.Upload
import Store.Types (MonadStorage)
import Store.Asset
import HTTP.Form.Deform
import HTTP.Path.Parser
import Action.Response
import Action
import Controller.Paths
import Controller.Form
import Controller.Volume

fileSizeForm :: DeformHandler f Int64
fileSizeForm = deformCheck "Invalid file size." (0 <) =<< deform

data UploadStartRequest =
    UploadStartRequest BS.ByteString Int64

uploadStart :: ActionRoute (Id Volume)
uploadStart = action POST (pathJSON >/> pathId </< "upload") $ \vi -> withAuth $ do
  vol <- getVolume PermissionEDIT vi
  uploadStartRequest <- runForm Nothing $ UploadStartRequest
    <$> ("filename" .:> (deformCheck "File format not supported." (isJust . getFormatByFilename) =<< deform))
    <*> ("size" .:> (deformCheck "File too large." ((maxAssetSize >=) . fromIntegral) =<< fileSizeForm))
  tok <- createUploadSetSize vol uploadStartRequest
  return $ okResponse [] $ unId (view tok :: Id Token)

createUploadSetSize :: (MonadHas Entropy c m, MonadDB c m, MonadHasIdentity c m, MonadStorage c m) => Volume -> UploadStartRequest -> m Upload
createUploadSetSize vol (UploadStartRequest filename size) = do
    tok <- createUpload vol filename size
    file <- peeks $ uploadFile tok
    liftIO $ bracket
        (openFd file WriteOnly (Just 0o640) defaultFileFlags{ exclusive = True })
        closeFd
        (`setFdSize` COff size)
    pure tok

-- TODO: use this very soon
-- data UploadStartResponse = UploadStartResponse { unwrap :: Id Token }

data UploadChunkRequest =
    UploadChunkRequest (Id Token) BS.ByteString Int64 Int64 Int64 Int64 Int64

chunkForm :: DeformHandler f (Upload, Int64, Word64)
chunkForm = do
  csrfForm
  up <- "flowIdentifier" .:> (lift . (maybeAction <=< lookupUpload) =<< deform)
  let z = uploadSize up
  "flowFilename" .:> (deformGuard "Filename mismatch." . (uploadFilename up ==) =<< deform)
  "flowTotalSize" .:> (deformGuard "File size mismatch." . (z ==) =<< fileSizeForm)
  c <- "flowChunkSize" .:> (deformCheck "Chunk size too small." (256 <=) =<< deform)
  n <- "flowTotalChunks" .:> (deformCheck "Chunk count mismatch." ((1 >=) . abs . (pred z `div` c -)) =<< deform)
  i <- "flowChunkNumber" .:> (deformCheck "Chunk number out of range." (\i -> 0 <= i && i < n) =<< pred <$> deform)
  let o = c * i
  l <- "flowCurrentChunkSize" .:> (deformCheck "Current chunk size out of range." (\l -> (c == l || i == pred n) && o + l <= z) =<< deform)
  -- TODO: populate filename, total size from request
  let _ = UploadChunkRequest ((tokenId . accountToken . uploadAccountToken) up) (uploadFilename up) z c n i l
  return (up, o, fromIntegral l)

uploadChunk :: ActionRoute ()
uploadChunk = action POST (pathJSON </< "upload") $ \() -> withAuth $ do
  (up, off, len) <- runForm Nothing chunkForm
  file <- peeks $ uploadFile up
  let checkLength n
        | n /= len = do
          t <- peek
          focusIO $ logMsg t ("uploadChunk: wrong size " ++ show n ++ "/" ++ show len)
          result $ response badRequest400 [] ("Incorrect content length: file being uploaded may have moved or changed" :: JSON.Value)
        | otherwise = return ()
  bl <- peeks Wai.requestBodyLength
  case bl of
    Wai.KnownLength l -> checkLength l
    _ -> return ()
  rb <- peeks Wai.requestBody
  n <- liftIO (writeChunk off len file rb)
  checkLength n -- TODO: clear block (maybe wait for calloc)
  return $ emptyResponse noContent204 []

-- | Write one contiguous block of data to a file
writeChunk
  :: Int64 -- ^ Offset to start writing block into
  -> Word64 -- ^ Length of block to be written
  -> RawFilePath -- ^ The target file to write into
  -> IO BS.ByteString -- ^ The data source that provides chunks of data for writing
  -> IO Word64 -- ^ number of bytes written
writeChunk off len file rb = bracket
    (openFd file WriteOnly Nothing defaultFileFlags)
    (\f -> putStrLn "closeFd..." >> closeFd f) $ \h -> do
      _ <- fdSeek h AbsoluteSeek (COff off)
      let block n = do
            b <- rb
            if BS.null b
              then do
                return n
              else do
                let n' = n + fromIntegral (BS.length b)
                    write b' = do
                      w <- BSU.unsafeUseAsCStringLen b' $ \(buf, siz) -> fdWriteBuf h (castPtr buf) (fromIntegral siz)
                      if w < fromIntegral (BS.length b')
                        then do
                          write $! BS.drop (fromIntegral w) b'
                        else do
                          block n'
                if n' > len
                  then do
                    return n'
                  else do
                    write b
      block 0

testChunk :: ActionRoute ()
testChunk = action GET (pathJSON </< "upload") $ \() -> withAuth $ do
  (up, off, len) <- runForm Nothing chunkForm
  file <- peeks $ uploadFile up
  r <- liftIO $ bracket
    (openFd file ReadOnly Nothing defaultFileFlags)
    closeFd $ \h -> do
    _ <- fdSeek h AbsoluteSeek (COff off)
    allocaArray bufsiz $ \buf -> do
      let block 0 = return False
          block n = do
            r <- fdReadBuf h buf $ n `min` fromIntegral bufsiz
            a <- peekArray (fromIntegral r) buf
            if r == 0
              then return False -- really should be error
              else if any (0 /=) a
                then return True
                else block $! n - r
      block (CSize len)
  return $ emptyResponse (if r then ok200 else noContent204) []
  where
  bufsiz = fromIntegral defaultChunkSize
