module Databrary.String
  ( toCamel
  ) where

import Data.Char (isUpper, toLower, toUpper)

toCamel :: String -> String
toCamel "" = ""
toCamel (c:s) = toUpper c:toCamel' s

toCamel' :: String -> String
toCamel' "" = ""
toCamel' ('_':s) = toCamel s
toCamel' (c:s) = c:toCamel' s
