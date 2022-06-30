{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  Conllu.Utils
-- Copyright   :  © 2018 bruno cuconato
-- License     :  LPGL-3
--
-- Maintainer  :  bruno cuconato <bcclaro+hackage@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- the library's utility functions.

-- TODO add export List

module Conllu.Utils where


-- import           Control.Exception.Base
-- import           Data.Char
import           Data.Maybe
-- import qualified Data.Text              as T

---
-- char functions
-- TODO (map toUpper doesn’t work for Unicode because the result could have a different length)

{-
upcaseStr :: String -> String
upcaseStr = map toUpper

downcaseStr :: String -> String
downcaseStr = map toLower


upcaseStr' :: Text -> Text
upcaseStr' =  toUpper

downcaseStr' :: Text -> Text
downcaseStr' =  toLower
-}

---
-- assertions

{-
assNothing :: Maybe a -> Bool -> Bool
assNothing m = assert (isNothing m)

assSomething :: Maybe a -> Bool -> Bool
assSomething m = assert (isJust m)

assNull :: [a] -> Bool -> Bool
assNull l = assert (null l)
-}

---
-- function tools
if' :: Bool -> a -> a -> a
if' True  x  _b = x
if' False _b y  = y

consIf :: (a -> Bool) -> a -> [a] -> [a]
-- | cons value if it satisfies the predicate, else do nothing.
consIf p a as =
  if p a
    then a : as
    else as

---
-- safe functions
safehead :: [a] -> Maybe a
safehead []    = Nothing
safehead (x:_) = Just x

safeRead :: Read a => String -> Maybe a
safeRead = fmap fst . listToMaybe . reads
