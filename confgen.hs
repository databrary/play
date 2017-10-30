#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [mtl text])" -i runhaskell

-- This is a databrary.conf generator
-- Pass the -deploy flag for a conf with deployment settings
-- Pass the -devlop flag for a conf with development settings
--TODO File needs to be in ByteString

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

import System.Process (callProcess)
import System.Directory ( doesDirectoryExist
                        , createDirectoryIfMissing
                        , doesFileExist
                        , getCurrentDirectory)
import System.Environment (getArgs)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 (unlines)
import qualified Data.ByteString.Char8 as ByteString8
import Data.Monoid ((<>))

main :: IO ()
main = do
  projectRoot <- getCurrentDirectory
  flags <- getArgs
  let bsProjectRoot = ByteString8.pack projectRoot
      databraryConf = "databrary.conf"
      generator x = createFileIfMissing databraryConf $ x bsProjectRoot
  case head flags of 
    "-deploy" -> generator confDeploy
    "-develop" -> generator confDevelop

confDeploy :: ByteString -> ByteString
confDeploy absPath = ByteString8.unlines 
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
  , "#comment out transcode dir when using nix-build"
  , "store {"
  , "  master = \"" <> absPath <> "/store\""
  , "  upload = \"" <> absPath <> "/upload\""
  , "  temp  = \"" <> absPath <> "/tmp\""
  , "  stage  = \"" <> absPath <> "/stage\""
  , "  cache  = \"" <> absPath <> "/cache\""
  , "  transcode {"
  , "    #dir = \"" <> absPath <> "/trans\""
  , "  }"
  , "}"
  , ""
  , "solr {"
  , "  run = true"
  , "  host = \"localhost\""
  , "  port = 8983"
  , "  home = \"" <> absPath <> "/solr\""
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

confDevelop :: ByteString -> ByteString
confDevelop absPath = ByteString8.unlines 
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
  , "  host = \"localhost\""
  , "  #sock = \"./databrary-nix-db/work/.s.PGSQL.5432\""
  , "  port = 5432"
  , "  user = \"databrary\""
  , "  #user = \"postgres\""
  , "  pass = \"databrary123\""
  , "  db = \"databrary\""
  , "  #db = \"postgres\""
  , "}"
  , ""
  , "#comment out transcode dir when using nix-build"
  , "store {"
  , "  master = \"" <> absPath <> "/store\""
  , "  upload = \"" <> absPath <> "/upload\""
  , "  temp  = \"" <> absPath <> "/tmp\""
  , "  stage  = \"" <> absPath <> "/stage\""
  , "  cache  = \"" <> absPath <> "/cache\""
  , "  transcode {"
  , "    dir = \"" <> absPath <> "/trans\""
  , "  }"
  , "}"
  , ""
  , "solr {"
  , "  run = true"
  , "  host = \"localhost\""
  , "  port = 8983"
  , "  home = \"" <> absPath <> "/solr\""
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
