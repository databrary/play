{-# LANGUAGE OverloadedStrings #-}
module Data.Csv.Contrib
  ( getHeaders
  , extractColumnsDistinctSample
  , extractColumnDefaulting
  , extractColumn
  ) where

import qualified Data.ByteString as BS
import qualified Data.Csv as CSV
import qualified Data.HashMap.Strict as HMP
import qualified Data.List as L
import qualified Data.Vector as V
import Data.Vector (Vector)

getHeaders :: CSV.Header -> [BS.ByteString]
getHeaders = V.toList

extractColumnsDistinctSample :: Int -> CSV.Header -> Vector CSV.NamedRecord -> [(BS.ByteString, [BS.ByteString])]
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

extractColumnDefaulting :: BS.ByteString -> Vector CSV.NamedRecord -> [BS.ByteString]
extractColumnDefaulting hdr records =
   extractColumn hdr records (maybe "" id)

extractColumn :: BS.ByteString -> Vector CSV.NamedRecord -> (Maybe BS.ByteString -> a) -> [a]
extractColumn hdr records applyDefault =
   ( V.toList
   . fmap (\rowMap -> (applyDefault . HMP.lookup hdr) rowMap))
   records
