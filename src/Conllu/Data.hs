module Conllu.Data where

---
-- imports
-- library
import Conllu.Type
--stdlib
import Data.Function
import Data.List
import Data.Maybe
import Data.Tree

-- relations
getRel :: (Token -> Bool) -> [Token] -> [(TTree,TTree)]
getRel p ts =
  let dts  = filter p ts
      tr   = sTksToTTree ts
      htrs = map (\t -> rmIxSubTree (_ix t) $ getIxSubTree (fromJust $ _dephead t) tr) dts
      dtrs = map (\t -> getIxSubTree (_ix t) tr) dts
  in zip dtrs htrs

getIxSubTree :: Index -> TTree -> TTree
getIxSubTree i tt = head $ getSubTreesBy (\t -> i == _ix t) tt

rmIxSubTree :: Index -> TTree -> TTree
rmIxSubTree i tt = fromJust $ rmSubTreesBy (\t -> i == _ix t) tt

getDepSubTrees :: Dep -> TTree -> [TTree]
getDepSubTrees d = getSubTreesBy (\t -> d == _dep t)

getDepTks :: Dep -> [Token] -> [Token]
getDepTks d = filter (\t -> d == _dep t)

linTTree :: TTree -> [Token]
linTTree = lin (compare `on` _ix)

tTreeToStr :: TTree -> String
tTreeToStr t =
  unwords $ map (fromMaybe "" . _form) $ linTTree t

---
-- generalized functions
getSubTreesBy :: (a -> Bool) -> Tree a -> [Tree a]
getSubTreesBy p t =
  if p . rootLabel $ t
    then t : tt
    else tt
  where
    tt = concatMap (getSubTreesBy p) $ subForest t

rmSubTreesBy :: (a -> Bool) -> Tree a -> Maybe (Tree a)
rmSubTreesBy p t = listToMaybe $ rmSubTreesBy' p t
  where
    rmSubTreesBy' :: (a -> Bool) -> Tree a -> [Tree a]
    rmSubTreesBy' p t =
      let root = rootLabel t
      in if p root
           then []
           else [Node root $ concatMap (rmSubTreesBy' p) $ subForest t]

lin :: (a -> a -> Ordering) -> Tree a -> [a]
lin f t = sortBy f $ flatten t

---
-- auxiliary functions
mapP :: (a -> b) -> (a, a) -> (b, b)
mapP f (a, a') = (f a, f a')
