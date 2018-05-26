-- |
-- Module      :  Conllu.DeprelTagset
-- Copyright   :  © 2018 bruno cuconato
-- License     :  LPGL-3
--
-- Maintainer  :  bruno cuconato <bcclaro+hackage@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- defines the CoNLL-U DEPREL tagset. this module is meant to be imported as
--
-- @
-- import qualified Conllu.DeprelTagset as D
-- @

module Conllu.DeprelTagset where

data EP
  = REF -- ^ only allowed in DEPS
  | ACL
  | ADVCL
  | ADVMOD
  | AMOD
  | APPOS
  | AUX
  | CASE
  | CCOMP
  | CC
  | CLF
  | COMPOUND
  | CONJ
  | COP
  | CSUBJ
  | DEP
  | DET
  | DISCOURSE
  | DISLOCATED
  | EXPL
  | FIXED
  | FLAT
  | GOESWITH
  | IOBJ
  | LIST
  | MARK
  | NMOD
  | NSUBJ
  | NUMMOD
  | OBJ
  | OBL
  | ORPHAN
  | PARATAXIS
  | PUNCT
  | REPARANDUM
  | ROOT
  | VOCATIVE
  | XCOMP
  deriving (Enum, Eq, Read, Show)
