{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Store.Transcoder
  ( runTranscoder
  , initTranscoder
  , transcodeEnabled
  ) where

import Data.Maybe (isJust)
import Data.Version (showVersion)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Paths_databrary (version, getDataFileName)
import Store.Types

runTranscoder :: Transcoder -> [String] -> IO (ExitCode, String, String)
runTranscoder (Transcoder cmd arg _) args =
  readProcessWithExitCode cmd (arg ++ args) ""

-- | Ensures the configured transcoder works, returning it as a capability. Will
-- throw a synchronous exception if the transcoder can't be run.
initTranscoder :: TranscoderConfig -> IO (Maybe Transcoder)
initTranscoder tconf@TranscoderConfig {..} = case (transcoderHost, transcoderDir) of
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

transcodeEnabled :: Storage -> Bool
transcodeEnabled = isJust . storageTranscoder
