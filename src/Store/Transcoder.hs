{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Store.Transcoder
  ( runTranscoder
  , initTranscoder
  , transcodeEnabled
  -- * Replacing initTranscoder
  , initTranscoder2
  , TranscodeConfig (..)
  ) where

import Data.Maybe (isJust)
import Data.Version (showVersion)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Paths_databrary (version, getDataFileName)
import qualified Store.Config as C
import Store.Types

runTranscoder :: Transcoder -> [String] -> IO (ExitCode, String, String)
runTranscoder (Transcoder cmd arg) args =
  readProcessWithExitCode cmd (arg ++ args) ""

data TranscodeConfig = TranscodeConfig
    { transcodeHost :: Maybe String
    , transcodeDir :: Maybe String
    , transcodeMount :: Maybe String
    }

initTranscoder2 :: TranscodeConfig -> IO (Maybe Transcoder)
initTranscoder2 TranscodeConfig {..} = case (transcodeHost, transcodeDir) of
    (Nothing, Nothing) -> return Nothing
    _ -> Just <$> do
        cmd <- getDataFileName "transctl.sh"
        let t =
                Transcoder cmd
                    $ ["-v", showVersion version]
                    ++ maybe [] (\d -> ["-d", d]) transcodeDir
                    ++ maybe [] (\h -> ["-h", h]) transcodeHost
                    ++ maybe [] (\m -> ["-m", m]) transcodeMount
        (r, out, err) <- runTranscoder t ["-t"]
        case r of
            ExitSuccess -> return t
            ExitFailure e ->
                fail
                    ("initTranscoder test: "
                    ++ show e
                    ++ "\n=== STDOUT ===\n"
                    ++ out
                    ++ "\n=== STDERR ===\n"
                    ++ err)

{-# DEPRECATED initTranscoder "Gradually being replaced by initTranscoder2" #-}
initTranscoder :: C.Config -> IO (Maybe Transcoder)
initTranscoder conf =
  case (host, dir) of
    (Nothing, Nothing) -> return Nothing
    _ -> Just <$> do
      cmd <- getDataFileName "transctl.sh"
      let t = Transcoder cmd $
                [ "-v", showVersion version ]
                ++ maybe [] (\d -> ["-d", d]) dir
                ++ maybe [] (\h -> ["-h", h]) host
                ++ maybe [] (\m -> ["-m", m]) mount
      (r, out, err) <- runTranscoder t ["-t"]
      case r of
        ExitSuccess -> return t
        ExitFailure e ->
            fail
                ("initTranscoder test: "
                ++ show e
                ++ "\n=== STDOUT ===\n"
                ++ out
                ++ "\n=== STDERR ===\n"
                ++ err)
  where
  host = conf C.! "host"
  dir = conf C.! "dir"
  mount = conf C.! "mount"

transcodeEnabled :: Storage -> Bool
transcodeEnabled = isJust . storageTranscoder
