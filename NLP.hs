module NLP where

---
-- imports
-- library
import UD
--stdlib
import Data.Function
import Data.List
import Data.Maybe
import Data.Tree

-- relations
getRel :: Dep -> TTree -> [(TTree,TTree)]
getRel dr tt =
  let apts = getDepSubTrees dr tt
      hts = map getHeadSubTree apts
  in zip apts hts
  where
    getHeadSubTree t =
      let i = fromJust $ dephead . rootLabel $ t
      in getIxSubTree i tt

getIxSubTree :: Index -> TTree -> TTree
getIxSubTree i tt = head $ getSubTreesBy (\t -> i == ix t) tt

getDepSubTrees :: Dep -> TTree -> [TTree]
getDepSubTrees dr = getSubTreesBy (\t -> dr == dep t)

linTTree :: TTree -> [Token]
linTTree = lin (compare `on` ix)

ttreeToStr :: TTree -> String
ttreeToStr t =
  unwords $ map (fromMaybe "" . form) $ linTTree t

---
-- generalized functions
getSubTreesBy :: (a -> Bool) -> Tree a -> [Tree a]
getSubTreesBy f t =
  if f . rootLabel $ t
    then t : tt
    else tt
  where
    tt = concatMap (getSubTreesBy f) $ subForest t

lin :: (a -> a -> Ordering) -> Tree a -> [a]
lin f t = sortBy f $ flatten t

---
-- auxiliary functions
mapP :: (a -> b) -> (a, a) -> (b, b)
mapP f (a, a') = (f a, f a')
