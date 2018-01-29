module Conllu.Diff where

import Conllu.Data.Tree
import Conllu.IO hiding (main)
import Conllu.Type
import Conllu.Utils

import Data.Foldable
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

diffField ::
     String -> (Token -> String) -> Token -> Token -> [FieldDiff]
diffField l f t1 t2 =
  maybe [] (\d -> (l, d) : []) $ (diffV `on` f) t1 t2

diffSTkOn :: [String] -> Token -> Token -> [FieldDiff]
diffSTkOn ls t1 t2 = foldMap (\(l, f) -> diffField l f t1 t2) lfs
  where
    lfs = filter (\(l,_) -> l `elem` ls) dfs
    dfs =
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

diffSTks :: [String] ->[Token] -> [Token] -> [TokenDiff]
diffSTks ls sts1 sts2 =
  if'
    (((==) `on` length) sts1 sts2)
    (let tis = map _ix sts1
     in zip tis $ zipWithM (\t1 t2 -> diffSTkOn ls t1 t2) sts1 sts2)
    []

diffSent :: Sentence -> Sentence -> Maybe SentDiff
diffSent s1 s2 =
  let dtks = diffSTks (sentSTks s1) (sentSTks s2)
  in if null dtks
       then Nothing
       else Just (sentId s1, dtks)

diffSents :: [Sentence] -> [Sentence] -> [SentDiff]
diffSents []  _  = []
diffSents _ss [] = []
diffSents ss1@(s1:st1) ss2@(s2:st2) =
  case (comparing sentId s1 s2) of
    LT -> diffSents st1 ss2
    GT -> diffSents ss1 st2
    EQ -> (diffSent s1 s2) ?: diffSents st1 st2

diffDoc :: Document -> Document -> DocDiff
diffDoc d1 d2 =
  (_file d1 ++ "/" ++ _file d2, diffSents (_sents d1) (_sents d2))

---
-- auxiliary functions
diffV :: Eq a => a -> a -> Maybe (a, a)
diffV x y =
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
          d1 <- readFile fp1
          d2 <- readFile fp2
          putStrLn $ printDocDiff $ diffDoc d1 d2
          return ()
