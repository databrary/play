{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Store.Transcoder
  ( runTranscoder
  , initTranscoder
  , transcodeEnabled
  -- * Replacing initTranscoder
  , initTranscoder2
  ) where

import Data.Maybe (isJust)
import Data.Version (showVersion)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Paths_databrary (version, getDataFileName)
import qualified Store.Config as C
import Store.Types

runTranscoder :: Transcoder -> [String] -> IO (ExitCode, String, String)
runTranscoder (Transcoder cmd arg _) args =
  readProcessWithExitCode cmd (arg ++ args) ""

initTranscoder2 :: TranscoderConfig -> IO (Maybe Transcoder)
initTranscoder2 tconf@TranscoderConfig {..} = case (transcoderHost, transcoderDir) of
    (Nothing, Nothing) -> return Nothing
    _ -> Just <$> do
        cmd <- getDataFileName "transctl.sh"
        let t =
                Transcoder cmd
                    (["-v", showVersion version]
                    ++ maybe [] (\d -> ["-d", d]) transcoderDir
                    ++ maybe [] (\h -> ["-h", h]) transcoderHost
                    ++ maybe [] (\m -> ["-m", m]) transcoderMount
                    )
                    tconf
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
      let t = Transcoder cmd
                ([ "-v", showVersion version ]
                ++ maybe [] (\d -> ["-d", d]) dir
                ++ maybe [] (\h -> ["-h", h]) host
                ++ maybe [] (\m -> ["-m", m]) mount)
                (TranscoderConfig host dir mount)
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
