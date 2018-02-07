{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Templates
  ( generateTemplatesJS
  ) where

import Control.Monad (guard, unless, forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isSpace)
import Data.Monoid ((<>))
import System.IO (withFile, withBinaryFile, IOMode(ReadMode, WriteMode), hPutStrLn, hIsEOF, hFlush)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP
import Data.Word (Word8)
import Data.ByteString.Internal (c2w)

import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Files
import Databrary.Web.Generate

wordEscaped :: Char -> BP.BoundedPrim Word8
wordEscaped q =
  BP.condB (== c2w q) (backslash q) $
  BP.condB (== c2w '\\') (backslash '\\') $
  BP.condB (>= c2w ' ') (BP.liftFixedToBounded BP.word8) $
  BP.condB (== c2w '\n') (backslash 'n') $
  BP.condB (== c2w '\r') (backslash 'r') $
  BP.condB (== c2w '\t') (backslash 't') $
    BP.liftFixedToBounded $ (\c -> ('\\', ('u', fromIntegral c))) BP.>$< BP.char8 BP.>*< BP.char8 BP.>*< BP.word16HexFixed
  where
  backslash c = BP.liftFixedToBounded $ const ('\\', c) BP.>$< BP.char8 BP.>*< BP.char8

-- | Escape (but do not quote) a ByteString
escapeByteString :: Char -> BS.ByteString -> B.Builder
escapeByteString = BP.primMapByteStringBounded . wordEscaped

quoteByteString :: Char -> BS.ByteString -> B.Builder
quoteByteString q s = B.char8 q <> escapeByteString q s <> B.char8 q

processTemplate :: RawFilePath -> (BS.ByteString -> IO ()) -> IO ()
processTemplate f g = do
  fp <- liftIO $ unRawFilePath f
  withFile fp ReadMode go
  where
    go h = do
      e <- hIsEOF h
      unless e $ do
        l <- BS.hGetLine h
        g $ BSC.dropWhile isSpace l
        go h

generateTemplatesJS :: WebGenerator
generateTemplatesJS fo@(f, _) = do
  tl <- liftIO $ findWebFiles ".html"
  guard (not $ null tl)
  fp <- liftIO $ unRawFilePath $ webFileAbs f
  webRegenerate
    (withBinaryFile fp WriteMode $ \h -> do
      hPutStrLn h "app.run(['$templateCache',function(t){"
      forM_ tl $ \tf -> do
        BSB.hPutBuilder h $ BSB.string8 "t.put(" <> quoteByteString q (webFileRel tf) <> BSB.char8 ',' <> BSB.char8 q
        processTemplate (webFileAbs tf) $ \s -> do
          let j = escapeByteString q s
          BSB.hPutBuilder h j -- this is hanging
          hFlush h            -- without this!!!
        hPutStrLn h $ q : ");"
      hPutStrLn h "}]);")
    [] tl fo
  where q = '\''
