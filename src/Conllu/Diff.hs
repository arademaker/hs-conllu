-- |
-- Module      :  Conllu.Diff
-- Copyright   :  © 2018 bruno cuconato
-- License     :  LPGL-3
--
-- Maintainer  :  bruno cuconato <bcclaro+hackage@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Build a diff of CoNLL-U elements (documents, sentences, words). it
-- may show the diff (the print* functions return pairs of the
-- differing fields in two words) or return the word pairs for further
-- processing (the diff* functions). it expects paired sentences as
-- input, and a default pairing function is provided.
--
-- this module is useful for visualizing or debugging the processing
-- of CoNLL-U corpora. be sure that the sentences are well-paired, or
-- else it'll be -- as always -- garbage in, garbage out.

module Conllu.Diff where

import Conllu.Type
import Conllu.Utils

import Data.Maybe
import Data.Ord

---
-- * type synonims
-- | CoNLL-U field diff.
type FDiff = StringPair

-- | pair of different words.
type WDiff a = (CW a, CW a)
-- | list of different words in a sentence.
type SDiff a = [WDiff a]
-- | list of lists of different words in sentences.
type DDiff a  = [SDiff a]

---
-- * diffing functions
diffW :: WDiff a -> Bool
-- | 'True' if any word field pairs are mismatched.
diffW = any isJust . printFieldDiffs

diffWs :: [CW a] -> [CW a] -> [WDiff a]
-- | filters the different word pairs.
diffWs ws1 ws2 = filter diffW $ zip ws1 ws2

diffS :: (Sent, Sent) -> SDiff AW
-- | diffs the sentence pair's words.
diffS (s1, s2) = diffWs (_words s1) (_words s2)

diffSs :: [(Sent, Sent)] -> DDiff AW
-- | diffs the sentence pairs.
diffSs = fmap diffS

---
-- * auxiliary functions
showM :: Show a => Maybe a -> String
-- | shows a word field.
showM (Just x) = show x
showM Nothing  = "_"

---
-- * pairing functions
pairSentsBy ::
     (Sent -> Sent -> Ordering) -> [Sent] -> [Sent] -> [(Sent, Sent)]
-- | pairs sentences by some ordering of 'Sent'.
pairSentsBy _f []  _ss  = []
pairSentsBy _f _ss [] = []
pairSentsBy f ss1@(s1:st1) ss2@(s2:st2) =
  case f s1 s2 of
    LT -> pairSentsBy f st1 ss2
    GT -> pairSentsBy f ss1 st2
    EQ -> (s1, s2) : pairSentsBy f st1 st2

sentId :: Sent -> Index
-- | try to find an index in a sentence's metadata.
sentId s =
  let mid = lookup "sent_id " $ _meta s
      id = fromMaybe "0" mid
  in read id :: Index

pairSents :: [Sent] -> [Sent] -> [(Sent, Sent)]
-- | pair sentences by their sent_id, found in their metadata.
pairSents = pairSentsBy $ comparing sentId

---
-- * printing functions
printFieldDiffs :: WDiff a -> [Maybe StringPair]
-- | list of maybe differing fields in a pair of words.
printFieldDiffs (w1, w2) = fmap (diffField w1 w2) pfs
  where
    diffField w1 w2 pf =
      let pf1 = pf w1
          pf2 = pf w2
      in if pf1 /= pf2
           then Just (pf1, pf2)
           else Nothing
    pfs =
      [ showM . _form
      , showM . _lemma
      , showM . _upos
      , showM . _xpos
      , show  . _feats
      , showM . _rel
      , show  . _deps
      , showM . _misc
      ]

printWDiff :: WDiff a -> [StringPair]
-- | list of differing fields in a pair of words.
printWDiff = catMaybes . printFieldDiffs

printSDiff :: SDiff a -> [[StringPair]]
-- | list of differing words in a sentence.
printSDiff = fmap printWDiff

printDDiff :: DDiff a -> [[[StringPair]]]
-- | list of lists of differing words in sentences.
printDDiff = fmap printSDiff
