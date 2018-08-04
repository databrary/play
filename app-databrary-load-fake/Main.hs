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
    ctx <- baseCtxt
    Just auth <- runReaderT (lookupSiteAuthByEmail False "test@databrary.org") ctx
    let superAdminAcct = siteAccount auth
        superAdminCtx = setCtxtIdentInfo ctx auth
    _ <- (flip runReaderT) superAdminCtx (do
        addVolumeWithAccess superAdminAcct)
    print ("Finished adding fake data." :: String)

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
setCtxtIdentInfo :: Ctxt -> SiteAuth -> Ctxt
setCtxtIdentInfo ctx auth =
  let
      pid = (partyId . partyRow . accountParty . siteAccount) auth
      ident = fakeIdentSessFromAuth auth True
  in
      ctx
          { ctxPartyId = pid
          , ctxSiteAuth = auth
          , ctxIdentity = ident
          }

fakeIdentSessFromAuth :: SiteAuth -> Bool -> Identity
fakeIdentSessFromAuth a su =
    Identified
      (Session
         (AccountToken (Token (Id "id") (UTCTime (fromGregorian 2017 1 2) (secondsToDiffTime 0))) a)
         "verf"
         su)

baseCtxt :: IO Ctxt
baseCtxt = do
    cn <- loadPGDatabase >>= pgConnect
    pure
        (Ctxt
             { ctxConn = cn
             , ctxRequest = Wai.defaultRequest
             , ctxPartyId = undefined
             , ctxSiteAuth = undefined
             , ctxIdentity = undefined
             })

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

