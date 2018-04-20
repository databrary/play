{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString.Lazy.Char8 as BLC

main :: IO ()
main = do
    bdy <- BLC.getContents
    runCheck bdy

runCheck :: BLC.ByteString -> Assertion
runCheck bdy =
    BLC.all (`elem` [' ', '\n']) bdy @? "expected empty robots.txt"
