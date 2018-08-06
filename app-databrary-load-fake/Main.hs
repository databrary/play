{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.Trans.Reader
import Data.Time
import qualified Network.Wai as Wai

import Has
import Model.Audit (MonadAudit)
import Model.Id
import Model.Identity.Types
import Model.Party (lookupSiteAuthByEmail)
import Model.Party.Types
import Model.Token.Types
import Model.Volume.Types (Volume(..), VolumeRow(..), blankVolume)
import Model.Volume (addVolume)
import Model.VolumeAccess (setDefaultVolumeAccessesForCreated)
import Service.DB

main :: IO ()
main = do
    -- TODO add a test mode where this is wrapped in a transaction
    cn <- loadPGDatabase >>= pgConnect
    Just auth <- runReaderT (lookupSiteAuthByEmail False "test@databrary.org") cn
    let superAdminCtx =
            (\(pid, ident) ->
                 Ctxt
                     { ctxConn = cn
                     , ctxRequest = Wai.defaultRequest
                     , ctxPartyId = pid
                     , ctxSiteAuth = auth
                     , ctxIdentity = ident
                     })
            (extractIdentInfo auth)
        superAdminAcct = siteAccount auth
    _ <- (flip runReaderT) superAdminCtx (do
        addVolumeWithAccess superAdminAcct)
    putStrLn "Finished adding fake data."

-- make a volume
addVolumeWithAccess :: MonadAudit c m => Account -> m Volume
addVolumeWithAccess a = do
    let bv = blankVolume
        v =
            bv
                { volumeRow = (volumeRow bv)
                    { volumeName = "Vol 2"
                    , volumeBody = Just "Volume two body here"
                    , volumeAlias = Just "Vol alias"
                    }
              }
    v' <- addVolume v
    setDefaultVolumeAccessesForCreated (accountParty a) v'
    pure v'

-- build up parameters for size of data to generate from yaml file

-- make context
extractIdentInfo :: SiteAuth -> (Id Party, Identity)
extractIdentInfo auth =
  let
      pid = (partyId . partyRow . accountParty . siteAccount) auth
      ident = fakeIdentSessFromAuth auth True
  in
      (pid, ident)

fakeIdentSessFromAuth :: SiteAuth -> Bool -> Identity
fakeIdentSessFromAuth a su =
    Identified
      (Session
         (AccountToken (Token (Id "id") (UTCTime (fromGregorian 2017 1 2) (secondsToDiffTime 0))) a)
         "verf"
         su)

data Ctxt = Ctxt
    { ctxRequest :: Wai.Request
    -- ^ for MonadHasRequest
    , ctxConn :: DBConn
    -- ^ MonadDB
    , ctxPartyId :: Id Party
    -- ^ for MonadAudit
    , ctxSiteAuth :: SiteAuth
    , ctxIdentity :: Identity
    }

instance Has Wai.Request Ctxt where
    view = ctxRequest

instance Has DBConn Ctxt where
    view = ctxConn

instance Has (Id Party) Ctxt where
    view = ctxPartyId

instance Has SiteAuth Ctxt where
    view = ctxSiteAuth

instance Has Identity Ctxt where
    view = ctxIdentity

