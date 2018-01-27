module Conllu.Diff where

import Conllu.Type
import Conllu.Data.Tree
import Conllu.Utils

import Data.Function
import Data.List
import Data.Maybe
import Data.Ord

type FieldDiff = (String, (String, String))
type TokenDiff = (Index, [FieldDiff])
type SentDiff  = (Index, [TokenDiff])

diffSTkOn ::
     Token -> Token -> [String] -> [FieldDiff]
-- optimize me with assoc functions
diffSTkOn t1 t2 ls = filter (\(l,_) -> l `elem` ls) ts
  where
    ts = diffSTk t1 t2

diffSTk :: Token -> Token -> [FieldDiff]
diffSTk t1 t2 =
  let (ls, fs) = unzip kfs
  -- check if zip is right
  in filterMap (isJust . snd) (\d -> (fst d, fromJust $ snd d)) $
     zip ls $ map (\f -> (compareV `on` f) t1 t2) fs
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
  :: Sentence
  -> Sentence
  -> [String]
  -> [TokenDiff]
-- should I use monads here?
diffSTks s1 s2 ls =
  let sts1 = sentSTks s1
      sts2 = sentSTks s2
  in if'
       (((==) `on` length) sts1 sts2)
       (catMaybes $ zipWith (diffAux diffSTkOn ls _ix) sts1 sts2)
       []

diffAux ::
     (a -> a -> [String] -> [d])
  -> [String]
  -> (a -> c)
  -> a
  -> a
  -> Maybe (c, [d])
diffAux f ls p a1 a2 =
  let r = f a1 a2 ls
  in if null r
       then Nothing
       else Just (p a1, r)

diffSentOn :: Sentence -> Sentence -> [String] -> Maybe SentDiff
diffSentOn s1 s2 ls = diffAux diffSTks ls sentId s1 s2

diffSentsOn :: [Sentence] -> [Sentence] -> [String] -> [SentDiff]
diffSentsOn [] _ _  = []
diffSentsOn ss [] _ = []
diffSentsOn ss1@(s1:st1) ss2@(s2:st2) ls =
  case (comparing sentId s1 s2) of
    LT -> diffSentsOn st1 ss2 ls
    GT -> diffSentsOn ss1 st2 ls
    EQ -> (diffSentOn s1 s2 ls) ?: diffSentsOn st1 st2 ls

---
-- auxiliary functions
compareV :: Eq a => a -> a -> Maybe (a, a)
compareV x y =
  if x == y
    then Nothing
    else Just (x, y)

showM :: Show a => Maybe a -> String
showM (Just x) = show x
showM Nothing  = ""

sentId :: Sentence -> Index
sentId s =
  let mid = lookup "sent_id " $ _meta s
      id = fromMaybe "0" mid
  in read id :: Index
