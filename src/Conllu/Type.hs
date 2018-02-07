{-# LANGUAGE OverloadedStrings #-}
module Conllu.Type where

---
-- imports

import Control.Exception.Base
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import Data.Tree
import qualified Data.Text as T

---
-- type and data declarations
type Stream = T.Text

data Document = Document
  { _file      :: Stream
  , _sents     :: [Sentence]
  } deriving (Eq,Show)

data Sentence = Sentence
  { _meta :: [Comment]
  , _tokens :: [Token]
  } deriving (Eq, Show)

type Comment    = StreamPair
type StreamPair = (Stream, Stream)

data Token
  = SToken { _ix      :: Index
           , _form    :: Form
           , _lemma   :: Lemma
           , _upostag :: PosTag
           , _xpostag :: Xpostag
           , _feats   :: Feats
           , _dephead :: Dephead
           , _deprel  :: DepRel
           , _deps    :: Deps
           , _misc    :: Misc
           }
  | MToken { _ix    :: Index
           , _end   :: Index
           , _form  :: Form
           , _misc  :: Misc
           -- other values should be empty
           }
  | EToken { _ix       :: Index
           , _childIx  :: Index
           , _form     :: Form
           , _lemma    :: Lemma
           , _upostag  :: PosTag
           , _xpostag  :: Xpostag
           , _feats    :: Feats
           -- heads and deprels specified in deps
           , _deps     :: Deps
           , _misc     :: Misc
           }
  deriving (Eq, Show)

type Index   = Int
type IxSep   = Char
type Form    = Maybe T.Text
type Lemma   = Maybe T.Text
type PosTag  = Maybe Pos
type Xpostag = Maybe T.Text
type Feats   = [StreamPair]
type Dephead = Maybe Index
type DepRel  = Maybe (Dep,Subtype)
type Subtype = T.Text
type Deps    = [(Index,(Dep,Subtype))]
type Misc    = Maybe T.Text

_dep :: Token -> Maybe Dep
_dep = dep . _deprel
  where
    dep (Just (dr,_)) = Just dr
    dep _ = Nothing

depIs :: Dep -> Token -> Bool
depIs d = maybe False (\d' -> d == d') . _dep

data Dep
  = ACL
  | ADVCL
  | ADVMOD
  | AMOD
  | APPOS
  | AUXdr
  | CASE
  | CC
  | CCOMP
  | CLF
  | COMPOUND
  | CONJ
  | COP
  | CSUBJ
  | DEP
  | DETdr
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
  | PUNCTdr
  | REPARANDUM
  | ROOT
  | VOCATIVE
  | XCOMP
  deriving (Eq, Show)

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
  deriving (Eq, Show)

-- trees
type TTree  = Tree Token -- only STokens
-- data Tree a = Node a [Tree a]

type ETree = (TTree, [Token]) -- enhanced tree

---
-- constructor functions
mkDep :: Stream -> Dep
mkDep = mkDep' . upcaseStream
  where
    mkDep' "ACL"        = ACL
    mkDep' "ADVCL"      = ADVCL
    mkDep' "ADVMOD"     = ADVMOD
    mkDep' "AMOD"       = AMOD
    mkDep' "APPOS"      = APPOS
    mkDep' "AUX"        = AUXdr
    mkDep' "CASE"       = CASE
    mkDep' "CC"         = CC
    mkDep' "CCOMP"      = CCOMP
    mkDep' "CLF"        = CLF
    mkDep' "COMPOUND"   = COMPOUND
    mkDep' "CONJ"       = CONJ
    mkDep' "COP"        = COP
    mkDep' "CSUBJ"      = CSUBJ
    mkDep' "DEP"        = DEP
    mkDep' "DET"        = DETdr
    mkDep' "DISCOURSE"  = DISCOURSE
    mkDep' "DISLOCATED" = DISLOCATED
    mkDep' "EXPL"       = EXPL
    mkDep' "FIXED"      = FIXED
    mkDep' "FLAT"       = FLAT
    mkDep' "GOESWITH"   = GOESWITH
    mkDep' "IOBJ"       = IOBJ
    mkDep' "LIST"       = LIST
    mkDep' "MARK"       = MARK
    mkDep' "NMOD"       = NMOD
    mkDep' "NSUBJ"      = NSUBJ
    mkDep' "NUMMOD"     = NUMMOD
    mkDep' "OBJ"        = OBJ
    mkDep' "OBL"        = OBL
    mkDep' "ORPHAN"     = ORPHAN
    mkDep' "PARATAXIS"  = PARATAXIS
    mkDep' "PUNCT"      = PUNCTdr
    mkDep' "REPARANDUM" = REPARANDUM
    mkDep' "ROOT"       = ROOT
    mkDep' "VOCATIVE"   = VOCATIVE
    mkDep' "XCOMP"      = XCOMP

mkPos :: Stream -> Pos
mkPos = mkPos' . upcaseStream
  where
    mkPos' "ADJ"      = ADJ
    mkPos' "ADP"      = ADP
    mkPos' "ADV"      = ADV
    mkPos' "AUX"      = AUXpos
    mkPos' "CCONJ"    = CCONJ
    mkPos' "DET"      = DETpos
    mkPos' "INTJ"     = INTJ
    mkPos' "NOUN"     = NOUN
    mkPos' "NUM"      = NUM
    mkPos' "PART"     = PART
    mkPos' "PRON"     = PRON
    mkPos' "PROPN"    = PROPN
    mkPos' "PUNCT"    = PUNCTpos
    mkPos' "SCONJ"    = SCONJ
    mkPos' "SYM"      = SYM
    mkPos' "VERB"     = VERB
    mkPos' "X"        = X

-- tokens
mkToken :: Index -> Maybe IxSep -> Maybe Index -> Form -> Lemma
  ->  PosTag -> Xpostag -> Feats -> Dephead -> DepRel -> Deps
  -> Misc -> Token
mkToken i sep ci = case sep of
  Nothing  -> mkSToken i
  Just '-' -> mkMToken i (fromJust ci)
  Just '.' -> mkEToken i (fromJust ci)

mkSToken :: Index -> Form -> Lemma -> PosTag -> Xpostag
  -> Feats -> Dephead -> DepRel -> Deps -> Misc -> Token
mkSToken i fo l up xp fe h dr d m =
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

mkMToken :: Index ->  Index -> Form -> Lemma -> PosTag -> Xpostag
  -> Feats -> Dephead -> DepRel -> Deps -> Misc -> Token
mkMToken s e fo l up xp fe h dr d m =
  assert
    (mTokenOK fo l up xp fe h dr d)
    MToken {_ix = s, _end = e, _form = fo, _misc = m}

mkEToken :: Index ->  Index -> Form -> Lemma -> PosTag -> Xpostag
  -> Feats -> Dephead -> DepRel -> Deps -> Misc -> Token
mkEToken i ci fo l up xp fe h dr d m =
  EToken
  { _ix      = i
  , _childIx = ci
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
mTokenOK :: Form -> Lemma -> PosTag -> Xpostag -> Feats -> Dephead
  -> DepRel -> Deps -> Bool
mTokenOK fo l up xp fe h dr d =
  assSomething fo $
  assNothing l $
  assNothing up $
  assNothing xp $
  assNull fe $ assNothing h $ assNothing dr $ assNull d True

eTokenOK h dr d = assNothing h $ assNothing dr $ assSomething d True

---
-- utility functions
upcaseStream :: Stream -> Stream
upcaseStream = T.toUpper

assNothing :: Maybe a -> Bool -> Bool
assNothing m = assert (isNothing m)

assSomething :: Maybe a -> Bool -> Bool
assSomething m = assert (isJust m)

assNull :: [a] -> Bool -> Bool
assNull l = assert (null l)
