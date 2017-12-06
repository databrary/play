{-# LANGUAGE CPP, OverloadedStrings #-}
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (void)
import qualified Data.Aeson.Encoding as J
import Data.ByteString.Builder (hPutBuilder)
import Data.Either (partitionEithers)
import qualified System.Console.GetOpt as Opt
import System.Environment (getProgName, getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (stdout)
import qualified System.Process as PR (callProcess, callCommand)

import qualified Databrary.Store.Config as Conf
import Databrary.Service.Init (withService)
import Databrary.Context
import Databrary.Web.Rules (generateWebFiles)
import Databrary.Action (runActionRoute)
import Databrary.Routes (routeMap)
import Databrary.Routes.API (swagger)
import Databrary.Warp (runWarp)
import Databrary.EZID.Volume (updateEZID)


data Flag
  = FlagConfig FilePath
  | FlagWeb
  | FlagAPI
  | FlagEZID
  deriving (Eq)

opts :: [Opt.OptDescr Flag]
opts =
  [ Opt.Option "c" ["config"] (Opt.ReqArg FlagConfig "FILE") "Path to configuration file [./databrary.conf]"
  , Opt.Option "w" ["webgen"] (Opt.NoArg FlagWeb) "Generate web assets only"
  , Opt.Option "a" ["api"] (Opt.NoArg FlagAPI) "Output Swagger API documention"
  , Opt.Option "e" ["ezid"] (Opt.NoArg FlagEZID) "Update EZID DOIs"
  ]

flagConfig :: Flag -> Either FilePath Flag
flagConfig (FlagConfig f) = Left f
flagConfig f = Right f

main :: IO ()
main = do
  putStrLn "Starting Main..."
  prog <- getProgName
  args <- getArgs
  let (flags, args', err) = Opt.getOpt Opt.Permute opts args
      (configs, flags') = partitionEithers $ map flagConfig flags

  conf <- mconcat <$> mapM Conf.load (case configs of
    [] -> ["databrary.conf"]
    l -> l)
  case (flags', args', err) of
    ([FlagWeb], [], []) -> do
      putStrLn "generating files..." 
      void generateWebFiles
      PR.callProcess "gzip" ["--force", "--keep", "--fast", "web/constants.json"]
      PR.callProcess "gzip" ["--force", "--keep", "--fast", "web/all.min.js"]
      PR.callProcess "gzip" ["--force", "--keep", "--fast", "web/all.min.css"]
      PR.callCommand "find web -type f -iname *.svg -exec gzip --force --keep --fast {} \\;"
      putStrLn "finished generating web files..."
      exitSuccess
    ([FlagAPI], [], []) -> do
      putStrLn "put web builder..."
      hPutBuilder stdout $ J.fromEncoding $ J.value swagger
      putStrLn "finished web builder..."
      exitSuccess
    ([FlagEZID], [], []) -> do
      putStrLn "update EZID..."
      r <- withService False conf $ runContextM $ withBackgroundContextM updateEZID
      putStrLn "update EZID finished..."
      if r == Just True then exitSuccess else exitFailure
    ([], [], []) -> do 
      putStrLn "No flags or args...."
      return ()
    _ -> do
      mapM_ putStrLn err
      putStrLn $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTION...]") opts
      exitFailure

  putStrLn "evaluating routemap..."
  routes <- evaluate routeMap
  putStrLn "evaluating routemap...withService..."
  withService True conf $ \rc -> do
    -- used to run migrations on startup when not in devel mode
    -- should check migrations2 table for last migration against last entry in schema2 dir
    putStrLn "running warp"
    runWarp conf rc (runActionRoute routes rc)
