{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.AuthorizeTest where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Has
import Databrary.Model.Authorize
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Token
import TestHarness

-- session exercise various logic in Authorize
test_Authorize_examples :: TestTree
test_Authorize_examples = testCaseSteps "Authorize examples" $ \step -> do
    (authorizeExpires . selfAuthorize) nobodyParty @?= Nothing

    cn <- connectTestDb
    step "Given an admin user"
    let adminUser = nobodyParty { partyRow = (partyRow nobodyParty) { partyId = Id 7 } }
    step "When we look at its direct authorization on databrary site"
    Just auth <- runReaderT (lookupAuthorize ActiveAuthorizations adminUser rootParty) TestContext { ctxConn = cn }
    step "Then we expect the authorization to have site and member level of ADMIN"
    (authorizeAccess . authorization) auth @?= Access { accessSite' = PermissionADMIN, accessMember' = PermissionADMIN }

    withinTestTransaction (\cn2 -> do
        step "Given the databrary site group"
        let dbSite = rootParty
        step "When we grant a user as super admin"
        let ctx =
                TestContext { ctxConn = cn2, ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxRequest = defaultRequest }
            a = mkAccount "Smith" "Jake" "jake@smith.com"
        Just auth3 <-
            runReaderT
                (do
                     a2 <- addAccount a
                     let p = accountParty a2
                     Just auth2 <- lookupSiteAuthByEmail False "jake@smith.com"
                     changeAccount (auth2 { accountPasswd = Just "somehashval"})
                     changeAuthorize (makeAuthorize (Access PermissionADMIN PermissionADMIN) Nothing p dbSite)
                     lookupSiteAuthByEmail False "jake@smith.com")
                ctx
        step "Then we expect the user to have admin privileges on the databrary site"
        siteAccess auth3 @?= Access { accessSite' = PermissionADMIN, accessMember' = PermissionADMIN })

    withinTestTransaction (\cn2 -> do
        step "Given a superadmin"
        ctxt <-
            runReaderT
                (do
                     Just auth2 <- lookupSiteAuthByEmail False "test@databrary.org"
                     let pid = Id 7
                         ident = fakeIdentSessFromAuth auth2 True
                     pure (TestContext {
                                ctxConn = cn2
                              , ctxIdentity = ident
                              , ctxSiteAuth = view ident
                              , ctxPartyId = pid
                              , ctxRequest = defaultRequest
                              }))
                TestContext { ctxConn = cn }
        step "When the superadmin grants the institution admin access on the db site"
        let p = mkInstitution "New York University"
        authorization1 <-
            runReaderT
                (do
                     created <- addParty p
                     changeAuthorize (makeAuthorize (Access PermissionADMIN PermissionNONE) Nothing created rootParty)
                     -- TODO: what can an institution do on the site, if anything?
                     lookupAuthorization created rootParty
                )
                ctxt
        step "Then we expect the institution to have ADMIN site access, no member privileges"
        authorizeAccess authorization1 @?= Access { accessSite' = PermissionADMIN, accessMember' = PermissionNONE })
  
    -- create institution party (super admin ident)
    -- save authorize as EDIT on databrary (super admin ident)
    -- create AI account (identity not needed)
    -- update AI account (identity not needed)
    -- save authorize as EDIT on instituion (super admin ident or admin ident?)
    -- lookup AI account

    --- ... w/AI + affiliate; lookup affiliate account

    -- repeat scenarios above but perform a restricted action (create volume as AI, view volume as affiliate)
  
mkAccount :: T.Text -> T.Text -> BS.ByteString -> Account
mkAccount sortName preName email = 
    let pr = (partyRow blankParty) { partySortName = sortName , partyPreName = Just preName }
        p = blankParty { partyRow = pr, partyAccount = Just a }
        a = blankAccount { accountParty = p, accountEmail = email }
    in a

fakeIdentSessFromAuth :: SiteAuth -> Bool -> Identity
fakeIdentSessFromAuth a su =
    Identified
      (Session
         (AccountToken (Token (Id "id") (UTCTime (fromGregorian 2017 1 2) (secondsToDiffTime 0))) a)
         "verf"
         su)

mkInstitution :: T.Text -> Party
mkInstitution instName =
    blankParty {
          partyRow = (partyRow blankParty) { partySortName = instName }
        }

-- Distribution of typical auths (site / member):

--     admin/admin from each super admin to db group <<
--     admin/none from each institution party to db group <<

--     edit/none from each AI to their institution <<

--     none/edit from affiliate to AI (high) <<
--     read/admin from affiliate to AI (high) <<
--     read/read  from affiliate to AI (med) <<
--     read/edit from affiliate to AI (med) <<
--     none/admin from admin? to AI (low)
--     none/read  from collaborator? to AI (low)
--     read/none from collaborator? to AI (low)

--     none/none from affilaite to AI (med)
