module Model.VolumeState.Types
  ( VolumeStateKey
  , VolumeState(..)
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Text as T

import Model.Volume.Types
import Model.Id.Types

type VolumeStateKey = T.Text

data VolumeState = VolumeState
  { volumeStateKey :: !VolumeStateKey
  , volumeStateValue :: !JSON.Value
  , volumeStatePublic :: !Bool
  , stateVolumeId :: !(Id Volume)
  }
