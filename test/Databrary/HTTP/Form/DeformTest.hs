{-# LANGUAGE OverloadedStrings, TypeFamilies, ScopedTypeVariables #-}
module Databrary.HTTP.Form.DeformTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.HTTP.Form.Deform

tests :: TestTree
tests = testGroup "Databrary.HTTP.Form.DeformTest"
    [ testCase "textInteger-1" (textInteger "1" @?= Right (1::Int))
    
    ]

-- usages:
--   transFile <- runFormFiles [("file", maxAssetSize)] (api == HTML ?> htmlAssetEdit assetTarget) $ do
--      csrfForm
--      (file :: Maybe (FileInfo TempFile)) <- "file" .:> deform
--      upfile <- case file of Just f -> return Just (FileUploadForm f) ...
--      up <- mapM detectUpload upfile
--      return (transformed file)

--   fi <- runFormFiles [("json", 16*1024*1024) (Just $ htmlIngestForm v s) $ do
--      csrfForm
--      ("json" .:> do
--         (fileInfo :: FileInfo JSON.Value) <- deform
--         (deformCheck
--              "Must be JSON."
--              (\f -> checkAlt1 f || checkAlt2 (fileName f))
--              fileInfo)
--   r <- runIngest .... <$> ingestJSON ... (fileContent fi) ...

--   avatarInfo :: (..,..) <- runFormFiles [("avatar", maxAvaterSize)] (api == HTML ?> htmlPartyEdit p) $ do
--     (maybe (deformOptional $ return Nothing) (\a -> do
--       f <- deformCheck "Must be an image." formatIsImage =<<
--         deformMaybe' "Unknown or unsupported file format."
--         (getFormatByFilename (filename a))
--   _ <- forM avatarInfo $ mapM $ \(af, fmt) -> do
--      let fp = tempFilePath (fileContent af)
--      focusIO $ releaseTempFile $ fileContent af
--      return ()

-- exposed:
-- DeformT (monad, functor, applicative, alternative,
-- runDeform
-- deformSync'
-- .:>
-- withSubDeforms
-- deformOptional, ...
-- Deform(..)
-- deformError

-- deformRead
-- deformParse

-- dependencies:
-- Form
-- Wai.Parse.FileInfo
-- Form.Errors
-- Model.<Value types>


-- users of this module:
--  Databrary.Controller.Form
--     runFormFiles
--       params: FileContent f, RequestContext, FormHtml f, DeformActionM f a
--       uses: Action.Form.getFormData, runFormWith
--     csrfForm
--     runFormWith
--       params: FormData f, ...
--       uses: runFormView
