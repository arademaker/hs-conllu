-- |
-- Module      :  Conllu.UposTagset
-- Copyright   :  © 2018 bruno cuconato
-- License     :  LPGL-3
--
-- Maintainer  :  bruno cuconato <bcclaro+hackage@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- defines the CoNLL-U UPOS tagset. this module is meant to be imported as
--
-- @
-- import qualified Conllu.UposTagset as U
-- @

module Conllu.UposTagset where

data POS
  = ADJ
  | ADP
  | ADV
  | AUX
  | CCONJ
  | DET
  | INTJ
  | NOUN
  | NUM
  | PART
  | PRON
  | PROPN
  | PUNCT
  | SCONJ
  | SYM
  | VERB
  | X
  deriving (Enum, Eq, Read, Show)
