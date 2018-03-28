module Conllu.Data.Token where

---
-- imports
import Conllu.Type
import Conllu.Data.Tree
import Conllu.Utils

import Data.Function
import Data.List
import Data.Maybe
import Data.Tree

-- tokens
_depU :: Token -> Dep
_depU = mkU _dep

_depheadU :: Token -> TkIndex
_depheadU = mkU _dephead

-- relations
getRel :: (Token -> Bool) -> [Token] -> [(TTree, TTree)]
getRel p = getRel3 p
    (\tr t -> -- get subtree of head, with dependent subtree removed
       rmIxSubTree (_ix t) $ getIxSubTree (fromJust $ _dephead t) tr)
    (\tr t -> getIxSubTree (_ix t) tr) -- get dependent subtree

getIxSubTree :: TkIndex -> TTree -> TTree
getIxSubTree i tt = head $ getSubTreesBy (\t -> i == _ix t) tt

rmIxSubTree :: TkIndex -> TTree -> TTree
rmIxSubTree i tt = fromJust $ rmSubTreesBy (\t -> i == _ix t) tt

getDepSubTrees :: Dep -> TTree -> [TTree]
getDepSubTrees d = getSubTreesBy (depIs d)

getDepTks :: Dep -> [Token] -> [Token]
getDepTks d = filter $ depIs d

linTTree :: TTree -> [Token]
linTTree = lin (compare `on` _ix)

tTreeToStr :: TTree -> String
tTreeToStr t =
  unwords $ map (fromMaybe "" . _form) $ linTTree t

---
-- generalized functions
getRel3 :: (Token -> Bool) -> (TTree -> Token -> TTree)
  -> (TTree -> Token -> TTree)
  -> [Token] -> [(TTree, TTree)]
getRel3 p tf1 tf2 ts =
  let dts = filter p ts -- tks we want
      tr = sTksToTTree ts -- tree from tks
      htrs = map (tf1 tr) dts -- subtrees of head tks
      dtrs = map (tf2 tr) dts -- subtrees of dependent tks
  in zip htrs dtrs

getSubTreesBy :: (a -> Bool) -> Tree a -> [Tree a]
getSubTreesBy p t =
  if p . rootLabel $ t
    then t : tt
    else tt
  where
    tt = concatMap (getSubTreesBy p) $ subForest t

rmSubTreesBy :: (a -> Bool) -> Tree a -> Maybe (Tree a)
rmSubTreesBy p = foldTree a
  where
    a r f = -- Node r f : r = root, f = forest
      if p r
        then Nothing
        else Just (Node r $ catMaybes f)        

lin :: (a -> a -> Ordering) -> Tree a -> [a]
lin f t = sortBy f $ flatten t

mkU :: (a -> Maybe b) -> a -> b
mkU fm = fromJust . fm

---
-- auxiliary functions
mapP :: (a -> b) -> (a, a) -> (b, b)
mapP f (a, a') = (f a, f a')
