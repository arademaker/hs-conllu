-- |
-- Module      :  Conllu.Print
-- Copyright   :  © 2018 bruno cuconato
-- License     :  LPGL-3
--
-- Maintainer  :  bruno cuconato <bcclaro+hackage@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- prints CoNLL-U.

module Conllu.Print
  ( printDoc
  , printSent )
where

import Conllu.Type
import Conllu.Utils

import Data.List
import Data.Maybe
import Data.Semigroup
import Data.Monoid (Monoid(mempty, mappend))

-- TODO: use some kind of bi-directional thing to derive this module

-- | DiffList type from LYHGG
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (f . g)

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  a `mappend` b = a <> b

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

---
-- printing
printDoc :: Document -> String
-- | prints Document to a string.
printDoc d =
  fromDiffList . mconcat $
  map (\s -> printSent s `mappend` diffLSpace) $ _sents d

printSent :: Sentence -> DiffList Char
printSent ss =
  mconcat
    [ printComments (_meta ss)
    , diffLSpace
    , printTks (_tokens ss)
    ]

printComments :: [Comment] -> DiffList Char
printComments =
  toDiffList .
  intercalate "\n" .
  map
    (\(c1, c2) ->
       concat
         [ "# "
         , c1
         , if null c2
             then ""
             else "= " ++ c2
         ])

printTks :: [Token] -> DiffList Char
printTks = foldr (\t dl -> mconcat [printTk t, diffLSpace, dl]) mempty

printTk :: Token -> DiffList Char
printTk t = printTk' t
  where
    tkLine = toDiffList . intercalate "\t" . map (\f -> f t)
    printTk' t
      | isSToken t =
        tkLine
          [ printTkIx
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
      | isMTk t =
        tkLine
          [ printTkIx
          , printForm
          , emptyF
          , emptyF
          , emptyF
          , emptyF
          , emptyF
          , emptyF
          , emptyF
          , printMisc
          ]
      | otherwise =
        tkLine
          [ printTkIx
          , printForm
          , printLemma
          , printPostag
          , printXpostag
          , printFeats
          , emptyF
          , emptyF
          , printDeps
          , printMisc
          ]
    printMStr = fromMaybe "_"
    printTkIx tk = case _ix tk of
      SId ix -> show $ ix
      MId s e -> concat [show s, "-", show e]
      EId ix e -> concat [show ix, ".", show e]
    printForm = printMStr . _form
    printLemma = printMStr . _lemma
    printPostag = printPos . _upostag
    printXpostag = printMStr . _xpostag
    printFeats =
      printList
        (\(f, v) ->
           f ++
           if null v
             then ""
             else "=" ++ v) .
      _feats
    printDeps =
      printList (\(i, dr) -> show i ++ ":" ++ printDeprel dr) . _deps
    printDeprel (d, s) =
      downcaseStr (show d) ++
      if null s
        then ""
        else ":" ++ s
    printMisc = fromMaybe "_" . _misc
    emptyF t = "_"

printPos :: PosTag -> String
printPos (Just AUXpos) = "AUX"
printPos (Just DETpos) = "DET"
printPos (Just PUNCTpos) = "PUNCT"
printPos Nothing = "_"
printPos (Just _pos) = show _pos

printList :: (a -> String) -> [a] -> String
printList f = nullToStr . intercalate "|" . map f
  where
    nullToStr :: String -> String
    nullToStr xs =
      if null xs
        then "_"
        else xs

diffLSpace :: DiffList Char
diffLSpace = toDiffList "\n"
