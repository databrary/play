{-# LANGUAGE CPP, OverloadedStrings, TupleSections #-}
module Web.Cache
  ( lookupWebFile
  ) where

import Files
import Web
import Web.Types

#ifdef DEVEL
import Control.Arrow (first, right)
import Control.Concurrent.MVar (modifyMVar)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (runStateT)
import Data.Tuple (swap)

import Web.Rules

#else
import qualified Data.HashMap.Strict as HM

#endif

lookupWebFile :: RawFilePath -> Web -> IO (Either String (WebFilePath, WebFileInfo))
lookupWebFile f (Web wc _) = do
  wf <- makeWebFilePath f
#ifdef DEVEL
  modifyMVar wc $ \wm -> do
    swap . first (right (wf, )) <$>
      runStateT (runExceptT $ generateWebFile False wf) wm
#else
  return $ maybe (Left "") (Right . (wf, )) $ HM.lookup wf wc
#endif
