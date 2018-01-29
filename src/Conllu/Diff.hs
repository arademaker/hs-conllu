module Conllu.Diff where

import Conllu.Data.Tree
import Conllu.IO hiding (main)
import Conllu.Type
import Conllu.Utils

import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import Prelude hiding (readFile)
import System.Environment

type FieldDiff = (String, (String, String))
type TokenDiff = (Index, [FieldDiff])
type SentDiff  = (Index, [TokenDiff])
type DocDiff   = (String, [SentDiff])

diffSTkOn :: [String] -> Token -> Token -> [FieldDiff]
-- optimize me with assoc functions
diffSTkOn ls t1 t2 = filter (\(l,_) -> l `elem` ls) ts
  where
    ts = diffSTk t1 t2

diffSTk :: Token -> Token -> [FieldDiff]
diffSTk t1 t2 =
  filterMap (isJust . snd) (\d -> (fst d, fromJust $ snd d)) $
  map (\(l, f) -> (l, (compareV `on` f) t1 t2)) kfs
  where
    kfs =
      [ ("form", showM . _form)
      , ("lemma", showM . _lemma)
      , ("upostag", showM . _upostag)
      , ("xpostag", showM . _xpostag)
      , ("feats", show . _feats)
      , ("head", showM . _dephead)
      , ("deprel", showM . _deprel)
      , ("deps", show . _deps)
      , ("misc", showM . _misc)
      ]


diffSentOn'
  :: [String]
  -> Sentence
  -> Sentence
  -> [TokenDiff]
-- should I use monads here?
diffSentOn' ls s1 s2 =
  let sts1 = sentSTks s1
      sts2 = sentSTks s2
  in if'
       (((==) `on` length) sts1 sts2)
       (catMaybes $ zipWith (diffAux (diffSTkOn ls) _ix) sts1 sts2)
       []

diffAux
  :: (a -> a -> [d])
  -> (a -> c)
  -> a
  -> a
  -> Maybe (c, [d])
diffAux f p a1 a2 =
  let r = f a1 a2
  in if null r
       then Nothing
       else Just (p a1, r)

diffSentOn :: [String] -> Sentence -> Sentence -> Maybe SentDiff
diffSentOn ls s1 s2 = diffAux (diffSentOn' ls) sentId s1 s2

diffSentsOn :: [String] -> [Sentence] -> [Sentence] -> [SentDiff]
diffSentsOn ls [] _ = []
diffSentsOn _ ss [] = []
diffSentsOn ls ss1@(s1:st1) ss2@(s2:st2) =
  case (comparing sentId s1 s2) of
    LT -> diffSentsOn ls st1 ss2
    GT -> diffSentsOn ls ss1 st2
    EQ -> (diffSentOn ls s1 s2) ?: diffSentsOn ls st1 st2

diffDocOn :: [String] -> Document -> Document -> DocDiff
diffDocOn ls d1 d2 =
  (_file d1 ++ "/" ++ _file d2, diffSentsOn ls (_sents d1) (_sents d2))

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

---
-- print
printFieldDiff :: FieldDiff -> String
printFieldDiff (la, (f, f')) = concat [la, ":", f, "->", f', "\n"]

printTkDiff :: TokenDiff -> String
printTkDiff (ix, fs) =
  concat [show $ ix, "\n", concatMap printFieldDiff fs]

printSentDiff :: SentDiff -> String
printSentDiff (sid, ts) =
  concat [show $ sid, "-sent--\n", concatMap printTkDiff ts, "\n"]

printDocDiff :: DocDiff -> String
printDocDiff (fs, ss) =
  concat [fs, "-doc--\n\n", concatMap printSentDiff ss, "---"]

---
-- main
main :: IO ()
main = do fp1:fp2:ls <- getArgs
          d1 <- readConlluFile fp1
          d2 <- readConlluFile fp2
          putStrLn $ printDocDiff $ diffDocOn ls d1 d2
          return ()
