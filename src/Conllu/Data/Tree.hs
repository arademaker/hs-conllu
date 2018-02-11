module Conllu.Data.Tree where

import Conllu.Type

import Data.List
import qualified Data.Map.Strict as M
import Data.Tree
import Data.Maybe

isRoot :: Token -> Bool
isRoot = depIs ROOT

toETree :: Sentence -> ETree
toETree s =
  let (sts, mts) = sentTksByType s
  in (sTksToTTree sts, mts)

sTksToTTree :: [Token] -> TTree
sTksToTTree ts =
  let (Just rt) = find isRoot ts
      dm = tksDepMap ts
  in rootToTTree rt dm
  where
    rootToTTree :: Token -> M.Map Index [Token] -> TTree
    rootToTTree t m =
      Node t $
      map (`rootToTTree` m) $ fromMaybe [] $ M.lookup (_ix t) m

tksDepMap :: [Token] -> M.Map Index [Token]
-- Map parent_ix [children_tks]
tksDepMap = foldr mkDepMap M.empty
  where
    mkDepMap t m =
      let hi = fromJust $ _dephead t
          cs = M.lookup hi m
      in if isJust cs
           then M.insertWith (++) hi [t] m
           else M.insert hi [t] m

fromETree :: ETree -> [Token]
--[] sorted list not implemented (use insertBy for meta tokens)
fromETree et =
  let (tt, mts) = et
  in mts ++ flatten tt

drawTTree :: TTree -> String
drawTTree tt = drawTree $ fmap showSToken tt
  where
    showSToken SToken {_ix = ix, _form = (Just fo)} =
      (show ix :: String) ++ "_" ++ fo
