module Conllu.Utils where

import Data.Maybe

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

(?:) :: Maybe a -> [a] -> [a]
ma ?: as =
  case ma of
    (Just a) -> a : as
    Nothing  -> as

zipWithM :: Monoid c => (a -> b -> c) -> [a] -> [b] -> c
zipWithM _f []     _bs    = mempty
zipWithM _f _as    []     = mempty
zipWithM f  (a:at) (b:bt) = f a b `mappend` (zipWithM f at bt)

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
filterF p = foldMap (\a -> if p a then [a] else [])
