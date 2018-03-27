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
import Data.Decimal

---
-- type and data declarations
data Document = Document
  { _file      :: String
  , _sents     :: [Sentence]
  } deriving (Eq,Show)

data Sentence = Sentence
  { _meta :: [Comment]
  , _tokens :: [Token]
  } deriving (Eq, Show)

type Comment    = StringPair
type StringPair = (String, String)

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

data Index   = IntIndex Int
             | DecimalIndex Decimal
             | RangeIndex Int Int
             deriving (Eq, Show, Ord)
type IxSep   = Char
type Form    = Maybe String
type Lemma   = Maybe String
type PosTag  = Maybe Pos
type Xpostag = Maybe String
type Feats   = [StringPair]
type Dephead = Maybe Index
type DepRel  = Maybe (Dep,Subtype)
type Subtype = String
type Deps    = [(Index,(Dep,Subtype))]
type Misc    = Maybe String

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
type TTree  = Tree Token -- only STokens
-- data Tree a = Node a [Tree a]

type ETree = (TTree, [Token]) -- enhanced tree

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
mkToken :: Index -> Maybe IxSep -> Maybe Index -> Form -> Lemma
  ->  PosTag -> Xpostag -> Feats -> Dephead -> DepRel -> Deps
  -> Misc -> Token
mkToken i sep ci = case sep of
  Nothing  -> mkSTk i
  Just '-' -> mkMTk i (fromJust ci)
  Just '.' -> mkETk i (fromJust ci)

mkSTk :: Index -> Form -> Lemma -> PosTag -> Xpostag
  -> Feats -> Dephead -> DepRel -> Deps -> Misc -> Token
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

mkMTk :: Index ->  Index -> Form -> Lemma -> PosTag -> Xpostag
  -> Feats -> Dephead -> DepRel -> Deps -> Misc -> Token
mkMTk s e fo l up xp fe h dr d m =
  assert
    (mTkOK fo l up xp fe h dr d)
    MToken {_ix = s, _end = e, _form = fo, _misc = m}

mkETk :: Index ->  Index -> Form -> Lemma -> PosTag -> Xpostag
  -> Feats -> Dephead -> DepRel -> Deps -> Misc -> Token
mkETk i ci fo l up xp fe h dr d m =
  assert (eTkOK h dr d)
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
mTkOK :: Form -> Lemma -> PosTag -> Xpostag -> Feats -> Dephead
  -> DepRel -> Deps -> Bool
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

sentSTks :: Sentence -> [Token]
sentSTks = fst . sentTksByType
