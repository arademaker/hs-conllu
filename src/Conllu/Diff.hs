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
import System.Environment

type TokenDiff = (Token, Token)
type SentDiff  = (Index, [TokenDiff])
type DocDiff   = (String, [SentDiff])

diffSTk :: Token -> Token -> Bool
diffSTk t1 t2 = any (\f -> f t1 == f t2) dfs
  where
    dfs =
      [ showM . _form
      , showM . _lemma
      , showM . _upostag
      , showM . _xpostag
      , show  . _feats
      , showM . _dephead
      , showM . _deprel
      , show  . _deps
      , showM . _misc
      ]

diffSTks :: [Token] -> [Token] -> [TokenDiff]
diffSTks sts1 sts2 =
  if'
    (length sts1 == length sts2)
    (filter (uncurry diffSTk) $ zip sts1 sts2)
    []

diffSent :: Sentence -> Sentence -> Maybe SentDiff
diffSent s1 s2 =
  let dtks = diffSTks (sentSTks s1) (sentSTks s2)
      sid  = sentId s1
  in if null dtks
       then Nothing
       else Just (sid, dtks)

diffSents :: [Sentence] -> [Sentence] -> [SentDiff]
diffSents []  _  = []
diffSents _ss [] = []
diffSents ss1@(s1:st1) ss2@(s2:st2) =
  case comparing sentId s1 s2 of
    LT -> diffSents st1 ss2
    GT -> diffSents ss1 st2
    EQ -> diffSent s1 s2 ?: diffSents st1 st2

diffDoc :: Document -> Document -> DocDiff
diffDoc d1 d2 =
  (_file d1 ++ "/" ++ _file d2, diffSents (_sents d1) (_sents d2))

---
-- auxiliary functions
showM :: Show a => Maybe a -> String
showM (Just x) = show x
showM Nothing  = "_"

sentId :: Sentence -> Index
sentId s =
  let mid = lookup "sent_id " $ _meta s
      id = fromMaybe "0" mid
  in read id :: Index

---
-- main
main :: IO ()
main = do [fp1,fp2] <- getArgs
          d1 <- readConlluFile fp1
          d2 <- readConlluFile fp2
          print $ diffDoc d1 d2
          return ()
