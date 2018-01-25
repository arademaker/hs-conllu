module Conllu.Data.Token where

---
-- imports
import Conllu.Type
import Conllu.Data.Tree
import Conllu.Utils

import Control.Exception.Base
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Tree

-- tokens
_depU :: Token -> Dep
_depU = mkU _dep

_depheadU :: Token -> Index
_depheadU = mkU _dephead

diffSTkWith ::
     Token -> Token -> [String] -> [(String, (String, String))]
-- optimize me with assoc functions
diffSTkWith t1 t2 ls = filter (\(l,_) -> l `elem` ls) ts
  where
    ts = diffSTk t1 t2

diffSTk :: Token -> Token -> [(String, (String, String))]
diffSTk t1 t2 =
  let (ls, fs) = unzip kfs
  in zip ls $ mapMaybe (\f -> (compareV `on` f) t1 t2) fs
  where
    kfs =
      [ ("form"    , showM . _form)
      , ("lemma"   , showM . _lemma)
      , ("upostag" , showM . _upostag)
      , ("xpostag" , showM . _xpostag)
      , ("feats"   , show . _feats)
      , ("head"    , showM . _dephead)
      , ("deprel"  , showM . _deprel)
      , ("deps"    , show . _deps)
      , ("misc"    , showM . _misc)
      ]

diffSTks
  :: [Token]
  -> [Token]
  -> [String]
  -> [(Index, [(String, (String, String))])]
-- should I use monads here?
diffSTks ts1 ts2 ls =
  if'
    (((==) `on` length) ts1 ts2)
    (concat $ zipWith diffIxSTkWith ts1 ts2)
    []
  where
    diffIxSTkWith t1 t2 =
      if'
        (((==) `on` _ix) t1 t2)
        (let d = diffSTkWith t1 t2 ls
         in if null d
              then []
              else [(_ix t1, d)])
        []

-- relations
getRel :: (Token -> Bool) -> [Token] -> [(TTree, TTree)]
getRel p = getRel3 p
    (\tr t -> -- get subtree of head, with dependent subtree removed
       rmIxSubTree (_ix t) $ getIxSubTree (fromJust $ _dephead t) tr)
    (\tr t -> getIxSubTree (_ix t) tr) -- get dependent subtree

getIxSubTree :: Index -> TTree -> TTree
getIxSubTree i tt = head $ getSubTreesBy (\t -> i == _ix t) tt

rmIxSubTree :: Index -> TTree -> TTree
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

compareV :: Eq a => a -> a -> Maybe (a, a)
compareV x y =
  if x == y
    then Nothing
    else Just (x, y)

showM :: Show a => Maybe a -> String
showM (Just x) = show x
showM Nothing  = ""
