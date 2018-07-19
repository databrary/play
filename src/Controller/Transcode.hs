{-# LANGUAGE OverloadedStrings #-}
module Controller.Transcode
  ( remoteTranscode
  -- , viewTranscodes
  , viewTranscodesHandler
  , TranscodeAction(..)
  , postTranscode
  ) where

import Control.Applicative (optional)
import Control.Monad (void)
import Data.Bits (shiftL, (.|.))
import Data.ByteArray (constEq)
import qualified Data.ByteString as BS
import Data.Char (isHexDigit, digitToInt)
import Data.List (stripPrefix)
import Data.Maybe (isNothing, mapMaybe)
import Data.Word (Word8)

import Ops
import Has (peeks)
import HTTP.Form.Deform
import HTTP.Path.Parser
import Action.Run
import Action
import Model.Id
import Model.Transcode
import Model.Asset
import Store.Transcode
import Controller.Paths
import Controller.Permission
import Controller.Form
import View.Transcode

unHex :: String -> Maybe [Word8]
unHex [] = Just []
unHex [_] = Nothing
unHex (h:l:r) = do
  hb <- unhex h
  lb <- unhex l
  ((shiftL hb 4 .|. lb) :) <$> unHex r
  where unhex x = isHexDigit x `thenUse` fromIntegral (digitToInt x)

sha1Form :: DeformHandler f BS.ByteString
sha1Form = do
  b <- deform
  deformGuard "Invalid SHA1 hex string" (length b == 40)
  maybe (deformError "Invalid hex string" >> return BS.empty) (return . BS.pack) $ unHex b

data RemoteTranscodeResultRequest =
    RemoteTranscodeResultRequest BS.ByteString (Maybe TranscodePID) Int (Maybe BS.ByteString) String

remoteTranscode :: ActionRoute (Id Transcode)
remoteTranscode = action POST (pathJSON >/> pathId) $ \ti -> withoutAuth $ do
  t <- maybeAction =<< lookupTranscode ti
  withReAuth (transcodeOwner t) $ do
    auth <- peeks $ transcodeAuth t
    RemoteTranscodeResultRequest _ _ res sha1 logs <- runForm Nothing $ do
      reqAuth <- "auth" .:> (deformCheck "Invalid authentication" (constEq auth :: BS.ByteString -> Bool) =<< deform)
      reqPid <- "pid" .:> (deformCheck "PID mismatch" (transcodeProcess t ==) =<< deformNonEmpty deform)
      RemoteTranscodeResultRequest
        <$> pure reqAuth
        <*> pure reqPid
        <*> ("res" .:> deform)
        <*> ("sha1" .:> optional sha1Form)
        <*> ("log" .:> deform)
    collectTranscode t res sha1 logs
    return $ okResponse [] BS.empty

viewTranscodes :: ActionRoute ()
viewTranscodes = action GET (pathHTML >/> "admin" >/> "transcode") $ \() -> viewTranscodesHandler

viewTranscodesHandler :: Action -- TODO: GET only
viewTranscodesHandler = withAuth $ do
  checkMemberADMIN
  t <- lookupActiveTranscodes
  peeks $ okResponse [] . htmlTranscodes t

data TranscodeAction
  = TranscodeStart
  | TranscodeStop
  | TranscodeFail
  deriving (Bounded, Enum)

instance Show TranscodeAction where
  show TranscodeStart = "start"
  show TranscodeStop = "stop"
  show TranscodeFail = "fail"

instance Read TranscodeAction where
  readsPrec _ s = mapMaybe (\t -> (,) t <$> stripPrefix (show t) s) $ enumFromTo minBound maxBound

instance Deform f TranscodeAction where
  deform = deformRead TranscodeStart

data UpdateTranscodeRequest = UpdateTranscodeRequest TranscodeAction

postTranscode :: ActionRoute (Id Transcode)
postTranscode = action POST (pathHTML >/> "admin" >/> pathId) $ \ti -> withAuth $ do
  t <- maybeAction =<< lookupTranscode ti
  UpdateTranscodeRequest act <- runForm Nothing $
    UpdateTranscodeRequest <$> ("action" .:> deform)
  case act of
    TranscodeStart | isNothing (transcodeProcess t) -> void $ startTranscode t
    TranscodeStop -> void $ stopTranscode t
    TranscodeFail | isNothing (assetSize $ assetRow $ transcodeAsset t) -> void $ changeAsset (transcodeAsset t){ assetRow = (assetRow $ transcodeAsset t){ assetSize = Just (-1) } } Nothing
    _ -> fail "Invalid action"
  peeks $ otherRouteResponse [] viewTranscodes ()
