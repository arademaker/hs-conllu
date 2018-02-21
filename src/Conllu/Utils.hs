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
ma ?: as =
  case ma of
    (Just a) -> a : as
    Nothing  -> as

zipWithNoid :: Monoid c => (a -> b -> c) -> [a] -> [b] -> c
zipWithNoid _f []     _bs    = mempty
zipWithNoid _f _as    []     = mempty
zipWithNoid f  (a:at) (b:bt) = f a b `mappend` zipWithNoid f at bt

filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap _p _f [] = []
filterMap p f (x:xt) =
  if p x
    then f x : filterMap p f xt
    else filterMap p f xt

deleteWith :: (a -> Bool) -> [a] -> [a]
deleteWith _p [] = []
deleteWith p (a:at) =
  if p a
    then at
    else a : deleteWith p at

filterF :: Foldable f => (a -> Bool) -> f a -> [a]
filterF p = foldMap (\a -> [a | p a])

equal :: Eq a => a -> a -> Bool
equal = (==)

applyWithLabel :: (a -> b) -> (c, a) -> (c, b)
applyWithLabel f (l, x) = (l, f x)
