module Databrary.Solr.StringUtil
  ( fromCamel
  ) where

import Data.Char (isUpper, toLower)

fromCamel :: String -> String
fromCamel "" = ""
fromCamel (c:s) = toLower c:fromCamel' s

fromCamel' :: String -> String
fromCamel' "" = ""
fromCamel' cs@(c:s)
  | isUpper c = '_':fromCamel cs
  | otherwise = c:fromCamel' s
