{-# LANGUAGE OverloadedStrings #-}
module Data.Csv.Contrib
  ( getHeaders
  , extractColumnsDistinctSample
  , extractColumnDefaulting
  , extractColumn
  , decodeCsvByNameWith
  , parseCsvWithHeader
  -- for testing only
  , repairCarriageReturnOnly
  ) where

import qualified Data.Attoparsec.ByteString as ATTO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as Csv
import qualified Data.Csv.Parser as Csv
import qualified Data.HashMap.Strict as HMP
import qualified Data.List as L
import qualified Data.Vector as V
import Data.Vector (Vector)

getHeaders :: Csv.Header -> [BS.ByteString]
getHeaders = V.toList

extractColumnsDistinctSample :: Int -> Csv.Header -> Vector Csv.NamedRecord -> [(BS.ByteString, [BS.ByteString])]
extractColumnsDistinctSample maxSamples hdrs records =
    zip hdrs'
        ( fmap
              ( getSample
              . (\hdr -> extractColumnDefaulting hdr records))
              hdrs' )
  where
    getSample :: [BS.ByteString] -> [BS.ByteString]
    getSample vals =
        (take maxSamples . L.nub) vals
    hdrs' :: [BS.ByteString]
    hdrs' = getHeaders hdrs

extractColumnDefaulting :: BS.ByteString -> Vector Csv.NamedRecord -> [BS.ByteString]
extractColumnDefaulting hdr records =
   extractColumn hdr records (maybe "" id)

extractColumn :: BS.ByteString -> Vector Csv.NamedRecord -> (Maybe BS.ByteString -> a) -> [a]
extractColumn hdr records applyDefault =
   ( V.toList
   . fmap (\rowMap -> (applyDefault . HMP.lookup hdr) rowMap))
   records

-- similar to decodeByName with except make parser parameter explicity
decodeCsvByNameWith :: (Csv.NamedRecord -> Csv.Parser a) -> BS.ByteString -> Either String (Csv.Header, Vector a)
decodeCsvByNameWith parseNamedRecord' contents = do
    -- Csv.decodeByNameWith Csv.defaultDecodeOptions contents
    (hdr, rcrds) <- parseCsvWithHeader contents
    let nRcrds = fmap Csv.toNamedRecord rcrds
    let rcrdsParser = traverse parseNamedRecord' nRcrds
    vals <- Csv.runParser rcrdsParser
    pure (hdr, vals)

parseCsvWithHeader :: BS.ByteString -> Either String (Csv.Header, Vector Csv.NamedRecord)
parseCsvWithHeader contents =
    runCsvParser ATTO.parseOnly contents

runCsvParser
    :: (ATTO.Parser (Csv.Header, Vector Csv.NamedRecord) -> BS.ByteString -> Either String (Csv.Header, Vector Csv.NamedRecord))
    -> BS.ByteString
    -> Either String (Csv.Header, Vector Csv.NamedRecord)
runCsvParser parse contents =
    parse (Csv.csvWithHeader Csv.defaultDecodeOptions) (repairCarriageReturnOnly contents)

-- | only fix newlines for bizarre macOS endings that use \r instead of \r\n
repairCarriageReturnOnly :: BS.ByteString -> BS.ByteString
repairCarriageReturnOnly contents =
    let
        hasNewline = BSC.elem '\n' contents
    in
        if hasNewline
        then contents
        else BSC.concatMap (\c -> if c == '\r' then "\r\n" else BSC.singleton c) contents
