{-# LANGUAGE OverloadedStrings #-}
module View.Paginate
  ( htmlPaginate
  ) where

import Control.Arrow (first)
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int32)
import Data.Maybe (catMaybes)
import qualified Network.Wai as Wai
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Ops
import Model.Paginate
import HTTP (encodePath')
import View.Html

take' :: Int32 -> [a] -> ([a], Bool)
take' _ [] = ([], False)
take' 0 _ = ([], True)
take' n (x:l) = first (x:) $ take' (pred n) l

paginateContent :: Paginate -> [a] -> (Maybe Paginate, [a], Maybe Paginate)
paginateContent (Paginate o l) x = ((o > 0) `thenUse` Paginate (o-l' `max` 0) l, x', m `thenUse` Paginate (o+l') l)
  where
  l' = pred l
  (x', m) = take' l' x

paginateLink :: Paginate -> Wai.Request -> H.Attribute
paginateLink (Paginate o l) q = HA.href $ builderValue $ encodePath' (Wai.pathInfo q) $
  filter ((`notElem` ["offset", "limit"]) . fst) (Wai.queryString q) ++ catMaybes
    [ (o == paginateOffset def) `unlessUse` ("offset", Just $ BSC.pack $ show o)
    , (l == paginateLimit  def) `unlessUse` ("limit", Just $ BSC.pack $ show l)
    ]

htmlPaginate :: ([a] -> H.Html) -> Paginate -> [a] -> Wai.Request -> H.Html
htmlPaginate f p c q = do
  f c'
  H.ul
    H.! HA.class_ "search-pages"
    $ do
      forM_ prev (\p' -> H.li $ H.a H.! paginateLink p' q $ "prev")
      forM_ next (\p' -> H.li $ H.a H.! paginateLink p' q $ "next")
  where (prev, c', next) = paginateContent p c
