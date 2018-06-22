module Controller.Transcode where

import Model.Id.Types
import Model.Transcode.Types
import Action

data TranscodeAction
  = TranscodeStart
  | TranscodeStop
  | TranscodeFail
instance Show TranscodeAction

remoteTranscode :: ActionRoute (Id Transcode)
postTranscode :: ActionRoute (Id Transcode)
