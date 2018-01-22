{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
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
import Conduit
import qualified Blaze.ByteString.Builder as BZB
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import qualified Data.Conduit.ByteString.Builder as CBB

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
       -- sink type used in zip: Sink ByteString (ResourceT IO) a == ConduitM ByteString Void (ResourceT IO) a

       -- examples 1
       writeFile "/tmp/input.txt" "First input here."
       runConduitRes (sourceFileBS "/tmp/input.txt" .| sinkFile "/tmp/output.txt")
       readFile "/tmp/output.txt" >>= putStrLn

       -- examples 2
       runConduit
         (  (yieldMany [1..] :: ConduitM () Int IO ())
         .| takeC 10
         .| (mapC (* 2) :: ConduitM Int Int IO ())
         .| takeWhileC (< 18)
         .| mapM_C print
         )

       -- stream from a source...
       -- instance ResponseData (Source IO (Flush BZB.Builder)) where
       --   response s h src = WAC.responseSource s h src
       -- Need to produce type: ConduitM () (Flush BZB.Builder) IO ()

       -- Use Blaze builder generally
       let bldr =
                (BZB.fromByteString ("abcefg" :: BS.ByteString))
             <> (BZB.fromByteString (" morehere." :: BS.ByteString))
             <> BZB.flush
             <> (BZB.fromByteString ("After flush." :: BS.ByteString))
       print (BZB.toByteString bldr)

       -- Use Conduit.Blaze which provides Conduit Builder m ByteString == ConduitM Builder ByteString m ()
       runConduit
         (  (yieldMany [bldr, bldr] :: ConduitM () BZB.Builder IO ())
         .| takeC 1
         -- .| mapC (\b -> BZB.toByteString b)
         .| CBB.builderToByteString
         .| mapM_C print
         )

       

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

