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

import qualified Conllu.DeprelTagset as D
import           Conllu.Type
import           Conllu.Utils

import           Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Monoid (Monoid(mempty, mappend))

-- TODO: use some kind of bi-directional thing to derive this module

-- | Functional list type from LYHGG, see HUGHES, RJM. "A novel
-- representation of lists and its application to the function
-- 'reverse'"
newtype FList a = FList { getFList :: [a] -> [a] }

instance Semigroup (FList a) where
  (FList f) <> (FList g) = FList (f . g)

instance Monoid (FList a) where
  mempty = FList (\xs -> [] ++ xs)
  a `mappend` b = a <> b

toFList :: [a] -> FList a
toFList xs = FList (xs++)

fromFList :: FList a -> [a]
fromFList (FList f) = f []

---
-- printing
printDoc :: Doc -> String
-- | prints a list of sentences to a string.
printDoc =
  fromFList . mconcat . map (\s -> printSent' s `mappend` diffLSpace)

printSent :: Sent -> String
-- | prints a sentence to a string.
printSent = fromFList . printSent'

printSent' :: Sent -> FList Char
printSent' ss =
  mconcat
    [ printComments (_meta ss)
    , diffLSpace
    , printWs (_words ss)
    ]

printComments :: [Comment] -> FList Char
printComments =
  toFList .
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

printWs :: [CW a] -> FList Char
printWs = foldr (\w dl -> mconcat [printW w, diffLSpace, dl]) mempty

printW :: CW a -> FList Char
printW = printW'
  where
    printW' :: CW a -> FList Char
    printW' w =
      wordLine w
        [ printID'
        , printFORM
        , printLEMMA
        , printUPOS'
        , printXPOS
        , printFEATS'
        , printHEAD
        , printDEPREL'
        , printDEPS'
        , printMISC
        ]
    wordLine :: CW a -> [CW a -> String] -> FList Char
    wordLine w = toFList . intercalate "\t" . map (\f -> f w)
    printID' = printID . _id
    printMStr = fromMaybe "_"
    printFORM = printMStr . _form
    printLEMMA = printMStr . _lemma
    printUPOS' = printUPOS . _upos
    printXPOS = printMStr . _xpos
    printFEATS' = printFEATS . _feats
    printHEAD = maybe "_" (printID . _head) . _rel
    printDEPREL' =
      maybe "_" (\r -> printDEPREL (_deprel r) (_subdep r)) . _rel
    printDEPS' = printDEPS . _deps
    printMISC = printMStr . _misc

---
-- field printers
printID :: ID -> String
printID id' =
  case id' of
    SID i -> show i
    MID s e -> concat [show s, "-", show e]
    EID i e -> concat [show i, ".", show e]

printUPOS :: UPOS -> String
printUPOS Nothing = "_"
printUPOS (Just pos) = show pos

printFEATS :: FEATS -> String
printFEATS = printList printFeat
  where
    printFeat Feat {_feat = f, _featValues = vs, _featType = mft} =
      let fts = maybe "" (\ft -> "[" ++ ft ++ "]") mft
      in concat [f, fts, "=", intercalate "," vs]

printDEPREL :: D.EP -> Maybe String -> String
printDEPREL dr sdr =
  downcaseStr $ show dr ++ maybe "" (":" ++) sdr

printDEPS :: DEPS -> String
printDEPS =
  printList
    (\r ->
       intercalate
         ":"
         ([printID (_head r), printDEPREL (_deprel r) (_subdep r)] ++
          fromMaybe [] (_rest r)))

---
-- utility printers
printList :: (a -> String) -> [a] -> String
printList f = nullToStr . intercalate "|" . map f
  where
    nullToStr :: String -> String
    nullToStr xs =
      if null xs
        then "_"
        else xs

diffLSpace :: FList Char
diffLSpace = toFList "\n"
