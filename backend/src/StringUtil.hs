module StringUtil
  (    fromCamel
  ) where

import Text.Casing

fromCamel :: String -> String
fromCamel = toQuietSnake . fromHumps
