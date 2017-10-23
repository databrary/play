#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [mtl text])" -i runhaskell

-- This is a databrary.conf generator
--TODO commented out components of example.conf must be added to
--confContent
--TODO File needs to be in ByteString

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

import System.Process (callProcess)
import System.Directory ( doesDirectoryExist
                        , createDirectoryIfMissing
                        , doesFileExist
                        , getCurrentDirectory)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 (unlines)
import qualified Data.ByteString.Char8 as ByteString8
import Data.Monoid ((<>))

main :: IO ()
main = do
  projectRoot <- getCurrentDirectory  
  let databraryConf = "databrary.conf"
  createFileIfMissing databraryConf $ confContent (ByteString8.pack projectRoot)

confContent :: ByteString -> ByteString
confContent absPath = ByteString8.unlines 
  [ "##See example.conf for complete conf options."
  , "secret = \"bob\""
  , "port = 8000"
  , "ssl {"
  , "}"
  , "log {"
  , "  messages {"
  , "    file = \"stderr\""
  , "   rotate = 100"
  , "  }"
  , "  access {"
  , "   file = \"stdout\""
  , "   rotate = 100"
  , "  }"
  , "}"
  , "#comment out host, user = databrary , db = databrary, and pass when using nix-build"
  , "db {"
  , "  #host = \"localhost\""
  , "  sock = \"./databrary-nix-db/work/.s.PGSQL.5432\""
  , "  port = 5432"
  , "  #user = \"databrary\""
  , "  user = \"postgres\""
  , "  #pass = \"databrary123\""
  , "  #db = \"databrary\""
  , "  db = \"postgres\""
  , "}"
  , ""
  , "store {"
  , "  master = \"" <> absPath <> "/store\""
  , "  upload = \"" <> absPath <> "/upload\""
  , "  temp  = \"" <> absPath <> "/tmp\""
  , "  stage  = \"" <> absPath <> "/stage\""
  , "  cache  = \"" <> absPath <> "/cache\""
  , "  transcode {"
  , "  }"
  , "}"
  , ""
  , "solr {"
  , "  run = true"
  , "  host = \"localhost\""
  , "  port = 8983"
  , " home = \"" <> absPath <> "/solr\""
  , "  core = \"databrary_core\""
  , " log  = \"" <> absPath <> "/databrary_logs/solr_log\""
  , "}"
  , "static {"
  , " authorize = \"bob@nyu.edu\""
  , " assist = \"bob@nyu.edu\""
  , "}"
  , "ezid {"
  , "}"
  , "notification {"
  , "  filter = \"*\""
  , "  copy = \"bob@nyu.edu\""
  , "}"
  ]
      
createFileIfMissing :: FilePath -> ByteString -> IO ()
createFileIfMissing aFile content =
  doesFileExist aFile >>= \case
    True -> putStrLn ("Skipping " ++ show aFile ++ "(already exist)")
    False -> ByteString.writeFile aFile content
