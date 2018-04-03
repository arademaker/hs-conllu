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

module Conllu.Utils where

import Control.Exception.Base
import Data.Char
import Data.Maybe

---
-- char functions
upcaseStr :: String -> String
upcaseStr = map toUpper

downcaseStr :: String -> String
downcaseStr = map toLower

---
-- assertions
assNothing :: Maybe a -> Bool -> Bool
assNothing m = assert (isNothing m)

assSomething :: Maybe a -> Bool -> Bool
assSomething m = assert (isJust m)

assNull :: [a] -> Bool -> Bool
assNull l = assert (null l)

---
-- function tools
if' :: Bool -> a -> a -> a
if' True  x  _b = x
if' False _b y  = y

(?:) :: Maybe a -> [a] -> [a]
-- | cons value if it is a Just, else do Nothing.
ma ?: as =
  case ma of
    (Just a) -> a : as
    Nothing  -> as

---
-- safe functions
safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:_) = Just x
