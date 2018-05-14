{-# LANGUAGE ScopedTypeVariables #-}
module Databrary.Store.ProbeTest where

import Test.Tasty.HUnit

import Databrary.Model.Format
import Databrary.Store.AV
import Databrary.Store.Probe

-- examples usage of various functions in Probe module
unit_Probe_examples :: Assertion
unit_Probe_examples = do
    avProbeCheckFormat videoFormat mkAVProbe @? "an mp4 probe result should be considered in standard video format"

-- TODO: generalize this to construct a variety of formats
mkAVProbe :: AVProbe
mkAVProbe =
    AVProbe {
          avProbeFormat = "mov,mp4,m4a,3gp,3g2,mj2"
        , avProbeStreams = (AVMediaTypeVideo,"h264"):[]
        }

{-
avProbeLength :: AVProbe -> Maybe Offset
avProbeLength AVProbe{ avProbeDuration = o } = (o > 0) `thenUse` (diffTimeOffset o)

data AVProbe = AVProbe
  { avProbeFormat :: BS.ByteString
  , avProbeDuration :: DiffTime
  , avProbeStreams :: [(AVMediaType, BS.ByteString)]
  , avProbeDate :: Maybe ZonedTime
  }
-}
