module Conllu.Utils where

import Data.Maybe

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

(?:) :: Maybe a -> [a] -> [a]
ma ?: as =
  case ma of
    (Just a) -> a : as
    Nothing -> as

filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap p f [] = []
filterMap p f (x:xt) =
  if p x
    then f x : filterMap p f xt
    else filterMap p f xt
