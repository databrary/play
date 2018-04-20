{-# LANGUAGE OverloadedStrings #-}
-- dependencies come implicitly from databary's haskell build-depends...

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as BLC

main :: IO ()
main = do
    resp <- httpLBS "http://localhost:8000/robots.txt"
    BLC.putStrLn (getResponseBody resp)
