{-# LANGUAGE CPP, OverloadedStrings #-}
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (void)
-- import qualified Data.Aeson.Encoding as J
-- import Data.ByteString.Builder (hPutBuilder)
import Data.Either (partitionEithers)
import qualified System.Console.GetOpt as Opt
import System.Environment (getProgName, getArgs)
import System.Exit (exitFailure)
-- import System.IO (stdout)

import qualified Databrary.Store.Config as Conf
import Databrary.Service.Init (withService)
import Databrary.Context
import Databrary.Web.Rules (generateWebFiles)
import Databrary.Action (runActionRoute)
import Databrary.Routes (routeMap)
import Databrary.Warp (runWarp)
import Databrary.EZID.Volume (updateEZID)

--- TEMPORARY
import System.Exit (exitSuccess)
import Control.Monad (when)

data Flag
  = FlagConfig FilePath
  | FlagWeb
  | FlagEZID
  deriving (Eq)

opts :: [Opt.OptDescr Flag]
opts =
  [ Opt.Option "c" ["config"] (Opt.ReqArg FlagConfig "FILE") "Path to configuration file [./databrary.conf]"
  , Opt.Option "w" ["webgen"] (Opt.NoArg FlagWeb) "Generate web assets only"
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

  when True
    (do
       print "use conduit"
       -- sink type used in zip: Sink ByteString (ResourceT IO) a
       -- stream a string to a sink:
       
       exitSuccess)
    
  startServer <- case (flags', args', err) of
    ([FlagWeb], [], []) -> do
      putStrLn "generating files..." 
      void generateWebFiles
      putStrLn "finished generating web files..."
      return False
    ([FlagEZID], [], []) -> do
      putStrLn "update EZID..."
      r <- withService False conf $ runContextM $ withBackgroundContextM updateEZID
      putStrLn "update EZID finished..."
      if r == Just True then return False else exitFailure
    ([], [], []) -> do 
      putStrLn "No flags or args...."
      return True
    _ -> do
      mapM_ putStrLn err
      putStrLn $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTION...]") opts
      exitFailure

  if startServer
  then do
    putStrLn "evaluating routemap..."
    routes <- evaluate routeMap
    putStrLn "evaluating routemap...withService..."
    withService True conf $ \rc -> do
      -- used to run migrations on startup when not in devel mode
      -- should check migrations2 table for last migration against last entry in schema2 dir
      putStrLn "running warp"
      runWarp conf rc (runActionRoute routes rc)
  else
    return ()

