-- |
-- Module      :  Conllu.Parse
-- Copyright   :  © 2018 bruno cuconato
-- License     :  LPGL-3
--
-- Maintainer  :  bruno cuconato <bcclaro+hackage@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- defines types for handling CoNLL-U data.

{-# LANGUAGE EmptyDataDecls #-}

module Conllu.Type where

---
-- imports
import Conllu.Utils

import Control.Exception.Base
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import Data.Tree

---
-- type and data declarations
data Sentence = Sentence
  { _meta    :: [Comment]  -- ^ the sentence's comments.
  , _words   :: [CWord SW] -- ^ the sentence's simple words.
  , _mtokens :: [CWord MT] -- ^ the sentence's multi-word tokens.
  , _enodes  :: [CWord EN] -- ^ the sentence's empty nodes.
  } deriving (Eq, Show)

-- | most comments are (key, value) pairs.
type Comment    = StringPair
type StringPair = (String, String)

-- | represents a word line in a CoNLL-U file. note that we have
-- collapsed some fields together: HEAD and DEPREL have been looped in
-- with DEPS, as their representation is the same; this way, the head
-- of the _deps field is the usual (HEAD, DEPREL) pair, and its tail
-- is the DEPS field found in the CoNLL-U file.
data CWord a = CWord
  { _ix      :: ID    -- ^ ID field
  , _form    :: FORM  -- ^ FORM field
  , _lemma   :: LEMMA -- ^ LEMMA field
  , _upostag :: UPOS  -- ^ UPOS field
  , _xpostag :: XPOS  -- ^ XPOS field
  , _feats   :: FEATS -- ^ FEATS field
  , _deps    :: DEPS  -- ^ DEPS field
  , _misc    :: MISC  -- ^ MISC field
  } deriving (Eq, Show)
-- [] make Ord instance after manual ID instance

data SW -- | simple word
-- | multiword token representation. do note that in MWTs only the ID,
-- FORM and MISC fields may be non-empty.
data MT
data EN -- | empty node


data ID
  = SId Index -- ^ word ID is an integer
  | MId Index
        Index -- ^ multi-word token ID is a range
  | EId Index
        Index -- ^ empty node ID is a decimal
  deriving (Eq, Show)
-- [] make manual Ord instance

type FORM  = Maybe String
type LEMMA = Maybe String
type UPOS  = Maybe Pos
type XPOS  = Maybe String
type FEATS = [StringPair] -- ^ features come in (key, value) pairs
type DEPS  = [Rel]
type MISC  = Maybe String

-- | dependency relation representation.
data Rel = Rel
  { _head   :: ID           -- ^ head ID
  , _deprel :: Dep          -- ^ dependency relation type
  , _subdep :: Maybe String -- ^ dependency relation subtype
  } deriving (Eq, Show)

type Index   = Int
type IxSep   = Char

_dep :: CWord SW -> Maybe Dep
-- | get DEPREL main value, if it exists.
_dep w = Just . _deprel =<< safehead (_deps w)

depIs :: Dep -> CWord SW -> Bool
depIs d = maybe False (\d' -> d == d') . _dep

data Dep
  = ACL
  | ADVCL
  | ADVMOD
  | AMOD
  | APPOS
  | AUX
  | CASE
  | CC
  | CCOMP
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
  deriving (Eq, Read, Show)

data Pos
  = ADJ
  | ADP
  | ADV
  | AUXpos -- pos because there is an aux in deprel
  | CCONJ
  | DETpos
  | INTJ
  | NOUN
  | NUM
  | PART
  | PRON
  | PROPN
  | PUNCTpos
  | SCONJ
  | SYM
  | VERB
  | X
  deriving (Eq, Read, Show)

-- trees
type TTree  = Tree (CWord SW)
-- data Tree a = Node a [Tree a]

type ETree = (TTree, [CWord SW]) -- enhanced tree

---
-- constructor functions
mkDep :: String -> Dep
mkDep = read . upcaseStr

mkPos :: String -> Pos
mkPos = mkPos' . upcaseStr
  where
    mkPos' "AUX" = AUXpos
    mkPos' "DET" = DETpos
    mkPos' "PUNCT" = PUNCTpos
    mkPos' pos = read pos

-- tokens
mkWord :: ID -> FORM -> LEMMA -> UPOS -> XPOS -> FEATS -> DEPS -> MISC
  -> CWord a
mkWord ix = case ix of
  SId _   -> mkSTk ix
  MId _ _ -> mkMTk ix
  EId _ _ -> mkETk ix

mkSTk :: ID -> FORM -> LEMMA -> UPOS -> XPOS
  -> FEATS -> DEPS -> MISC -> CWord SW
mkSTk i fo l up xp fe h dr d m =
  SToken { _ix      = i
         , _form    = fo
         , _lemma   = l
         , _upostag = up
         , _xpostag = xp
         , _feats   = fe
         , _dephead = h
         , _deprel  = dr
         , _deps    = d
         , _misc    = m
         }

mkMTk :: TkIndex -> Form -> Lemma -> PosTag -> Xpostag
  -> Feats -> Dephead -> DepRel -> Deps -> Misc -> Token
mkMTk s fo l up xp fe h dr d m =
  assert
    (mTkOK fo l up xp fe h dr d)
    MToken {_ix = s, _form = fo, _misc = m}

mkETk :: TkIndex -> Form -> Lemma -> PosTag -> Xpostag
  -> Feats -> Dephead -> DepRel -> Deps -> Misc -> Token
mkETk i fo l up xp fe h dr d m =
  assert (eTkOK h dr d)
  EToken
  { _ix      = i
  , _form    = fo
  , _lemma   = l
  , _upostag = up
  , _xpostag = xp
  , _feats   = fe
  , _deps    = d
  , _misc    = m
  }

---
-- validation
mTkOK :: FORM -> LEMMA -> UPOS -> XPOS -> FEATS -> (ID, (Dep, Maybe String))
  -> Bool
mTkOK fo l up xp fe h dr d =
  assSomething fo $
  assNothing l $
  assNothing up $
  assNothing xp $
  assNull fe $ assNothing h $ assNothing dr $ assNull d True

eTkOK :: Dephead -> DepRel -> Deps -> Bool
eTkOK h dr d =
  assNothing h $ assNothing dr $ (assert . not . null $ d) True

---
-- utility functions
tkOrd :: Token -> Token -> Ordering
-- | an ordering for tokens.
tkOrd t1 t2 =
  let c = (compare `on` _ix) t1 t2
  in case c of
       EQ -> sameIx t1 t2
       _ -> c
  where
    sameIx SToken {} _t = GT
    sameIx _t SToken {} = LT

actOnSentTks :: ([Token] -> [Token]) -> Sentence -> Sentence
actOnSentTks f s@Sentence{_tokens=tks} = s{_tokens=f tks}

actOnDocTks :: ([Token] -> [Token]) -> Document -> Document
actOnDocTks f d@Document {_sents = ss} =
  d {_sents = map (actOnSentTks f) ss}

sentTksByType :: Sentence -> ([Token],[Token])
-- ([SToken],[metaTokens:EToken,MToken])
sentTksByType Sentence{_tokens=ts} = partition isSToken ts

isSToken :: Token -> Bool
isSToken SToken{} = True
isSToken _        = False

isMTk :: Token -> Bool
isMTk MToken{} = True
isMTk _tk      = False

sTkIx :: Token -> Index
-- this should be safe
sTkIx SToken{_ix =(SId ix)} = ix

sentSTks :: Sentence -> [Token]
sentSTks = fst . sentTksByType
--}
