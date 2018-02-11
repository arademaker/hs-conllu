module Conllu.Print where

import Conllu.Type

import Data.List
import Data.Maybe
import Data.Monoid

-- TODO: use some kind of bi-directional thing to derive this module

---
-- DiffList type from LYHGG
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  (DiffList f) `mappend` (DiffList g) = DiffList (f . g) 

toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []  

---
-- printing
printDoc :: Document -> [String]
printDoc = fromDiffList . mconcat . map printSent . _sents

printSent :: Sentence -> DiffList String
printSent ss =
  mconcat
    [ printComments (_meta ss)
    , diffLSpace
    , printTks (_tokens ss)
    ]

printComments :: [Comment] -> DiffList String
printComments =
  toDiffList .
  intersperse "\n" .
  map
    (\(c1, c2) ->
       "# " ++
       c1 ++
       if null c2
         then ""
         else "= " ++ c2)

printTks :: [Token] -> DiffList String
printTks = foldr (\t dl -> mconcat [printTk t, diffLSpace, dl]) mempty

printTk :: Token -> DiffList String
printTk t = printTk' t
  where
    tkLine = toDiffList . intersperse "\t" . map (\f -> f t)
    printTk' t
      | isSToken t =
        tkLine
          [ printIx
          , printForm
          , printLemma
          , printPostag
          , printXpostag
          , printFeats
          , maybe "_" show . _dephead
          , maybe "_" printDeprel . _deprel
          , printDeps
          , printMisc
          ]
      | isMTk t = tkLine [printIx, show . _end, printForm, printMisc]
      | otherwise =
        tkLine
          [ printIx
          , show . _childIx
          , printForm
          , printLemma
          , printPostag
          , printXpostag
          , printFeats
          , printDeps
          , printMisc
          ]
    printMStr = fromMaybe "_"
    printIx = show . _ix
    printForm = printMStr . _form
    printLemma = printMStr . _lemma
    printPostag = printPos . _upostag
    printXpostag = printMStr . _xpostag
    printFeats = printList (uncurry (++)) . _feats
    printDeps =
      printList (\(i, dr) -> show i ++ ":" ++ printDeprel dr) . _deps
    printDeprel = (\(d, s) -> show d ++ ":" ++ s)
    printMisc = fromMaybe "_" . _misc

printPos :: PosTag -> String
printPos (Just AUXpos) = "AUX"
printPos (Just DETpos) = "DET"
printPos (Just PUNCTpos) = "PUNCT"
printPos Nothing = "_"
printPos (Just _pos) = show _pos

printList :: (a -> String) -> [a] -> String
printList f = nullToStr . concat . intersperse "|" . map f
  where
    nullToStr :: String -> String
    nullToStr xs =
      if null xs
        then "_"
        else xs

diffLSpace :: DiffList String
diffLSpace = toDiffList ["\n"]
