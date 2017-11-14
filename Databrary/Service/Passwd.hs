{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, TemplateHaskell #-}
module Databrary.Service.Passwd
  ( passwordPolicy
  , passwdCheck
  , Passwd
  , initPasswd
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import qualified Crypto.BCrypt as BCrypt
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Foreign.C.String (CString)
import Foreign.Ptr (nullPtr)

import Databrary.Ops
import Paths_databrary (getDataFileName)

passwordPolicy :: BCrypt.HashingPolicy
passwordPolicy = BCrypt.HashingPolicy
  { BCrypt.preferredHashAlgorithm = "$2b$"
  , BCrypt.preferredHashCost = 12
  }

foreign import ccall unsafe "crack.h FascistCheck"
  cracklibCheck :: CString -> CString -> IO CString

newtype Passwd = Passwd { _passwdLock :: MVar () }

initPasswd :: IO Passwd
initPasswd = Passwd <$> newMVar ()

passwdCheck :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Passwd -> IO (Maybe BS.ByteString)
passwdCheck passwd _ _ (Passwd lock) =
  withMVar lock $ \() ->
    BS.useAsCString passwd $ \p -> do
      pw_dict <- getDataFileName "cracklib/pw_dict"
      BS.useAsCString (BSC.pack pw_dict) $ \dict -> do
        r <- cracklibCheck p dict
        r /= nullPtr ?$> BS.packCString r
