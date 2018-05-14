{-# LANGUAGE ScopedTypeVariables #-}
module Databrary.Store.ProbeTest where

import Data.Either
import Data.Time
import Test.Tasty.HUnit

import Databrary.Model.Container
import Databrary.Model.Format
import Databrary.Model.Offset ()
import Databrary.Store.AV
import Databrary.Store.Probe
import TestHarness

-- examples usage of various functions in Probe module
unit_Probe_examples :: Assertion
unit_Probe_examples = do
    let csvFormat = getFormat' (Id 2)
    probeLength (ProbePlain csvFormat) @?= Nothing
    res <- runReaderT (probeFile "text/badformat" "/invalid/location") (TestContext {})
    isLeft res @? "probeFile should error on invalid format should error"
    -- TODO: what is the purpose of probeAutoPosition?
    off <-
        runReaderT
            (probeAutoPosition
                 (mkDatedContainer (fromGregorian 2017 1 2))
                 ((Just . mkDatedProbe) (mkZTime (fromGregorian 2017 1 3) 0)))
            (TestContext {})
    off @?= read "24:00:00.0"
    avProbeCheckFormat videoFormat mkAVProbe @? "an mp4 probe result should be considered in standard video format"

mkDatedContainer :: Day -> Container
mkDatedContainer d =
    Container {
          containerRow = ContainerRow { containerDate = Just d }
        }

mkZTime :: Day -> Integer -> ZonedTime
mkZTime d secs = utcToZonedTime utc (UTCTime d (secondsToDiffTime secs))
      
-- TODO: generalize this to construct a variety of formats
mkAVProbe :: AVProbe
mkAVProbe =
    AVProbe {
          avProbeFormat = "mov,mp4,m4a,3gp,3g2,mj2"
        , avProbeStreams = (AVMediaTypeVideo,"h264"):[]
        }

mkDatedProbe :: ZonedTime -> Probe
mkDatedProbe t =
    ProbeAV { probeAV = AVProbe { avProbeDate = Just t } }
