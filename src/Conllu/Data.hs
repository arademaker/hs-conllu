module Data where

---
-- imports
-- library
import Type
--stdlib
import Data.Function
import Data.List
import Data.Maybe
import Data.Tree

-- relations
getRel :: Dep -> TTree -> [(TTree,TTree)]
getRel dr tt =
  let apts = getDepSubTrees dr tt
      hts  = map getHeadSubTree apts
  in zip apts hts
  where
    getHeadSubTree t =
      let i = fromJust $ _dephead . rootLabel $ t
      in getIxSubTree i tt

getIxSubTree :: Index -> TTree -> TTree
getIxSubTree i tt = head $ getSubTreesBy (\t -> i == _ix t) tt

getDepSubTrees :: Dep -> TTree -> [TTree]
getDepSubTrees d = getSubTreesBy (\t -> d == _dep t)

getDepTks :: Dep -> [Token] -> [Token]
getDepTks d = filter (\t -> d == _dep t)

getRel' :: (Token -> Bool) -> [Token] -> [(TTree,TTree)]
getRel' p ts =
  let dts  = filter p ts
      tr   = stksToTTree ts
      htrs = map (\t -> getIxSubTree (fromJust $ _dephead t) tr) dts
      dtrs = map (\t -> getIxSubTree (_ix t) tr) dts
  in zip dtrs htrs

linTTree :: TTree -> [Token]
linTTree = lin (compare `on` _ix)

ttreeToStr :: TTree -> String
ttreeToStr t =
  unwords $ map (fromMaybe "" . _form) $ linTTree t

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
