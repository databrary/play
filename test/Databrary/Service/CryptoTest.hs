module Databrary.Service.CryptoTest where

import Data.ByteString.Char8 (pack)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure
import Test.Tasty.QuickCheck
import Control.Monad.Trans.Reader
import qualified Test.QuickCheck.Monadic as QCMonadic

import Databrary.Has
import Databrary.Service.Crypto
import Databrary.Service.Types (Secret(..))
import Databrary.Service.Entropy (Entropy, initEntropy)

data Context = Context
    { ctxSecret :: Secret
    , ctxEntropy :: Entropy
    }

instance Has Secret Context where
    view = ctxSecret

instance Has Entropy Context where
    view = ctxEntropy

prop_signAndUnsignRandomString :: String -> Property
prop_signAndUnsignRandomString str =
    let sec = Secret "jzx89cvbzx98cvz9z8x7cvz"
        bytes = pack str
    in
        QCMonadic.monadicIO $ do
            munsig <- QCMonadic.run $ do
                ent <- initEntropy
                sig <- runReaderT (sign bytes) (Context sec ent)
                runReaderT (unSign sig) (Context sec undefined)
            QCMonadic.assert (munsig == (Just bytes))

test_signAndUnsign :: [TestTree]
test_signAndUnsign =
    let sec = Secret "ajsd78fas78df7asdf7asd78fads78"
    in
        [ testCase "simple" $ do
            ent <- initEntropy
            sig <- runReaderT (sign "foobar") (Context sec ent)
            munsig <- runReaderT (unSign sig) (Context sec undefined)
            munsig @?= Just "foobar"
        , testCase "sign null string" $ do
            ent <- initEntropy
            sig <- runReaderT (sign "") (Context sec ent)
            munsig <- runReaderT (unSign sig) (Context sec undefined)
            munsig @?= Just ""
        ]


test_sign :: TestTree
test_sign =
    let sec = Secret "abcajhsd8f7yahdsfg789adshfg789adsryfg78adsgtyads"
    in
        expectFail -- Because "Nonce is not yet under test control"
        $ testCase "sign"
        $ do
              ent <- initEntropy
              sig <- runReaderT (sign "foobar") (Context sec ent)
              sig @?= "5jUOuWSd7dBGgrne8pD0bbea6Z5zsRfFunlebApSzusHgffoobar"
