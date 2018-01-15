module Conllu.Type where

{--

- TODO: validate input of mk*Token functions https://github.com/tonymorris/validation, https://github.com/mavenraven/validations, https://ro-che.info/articles/2015-05-02-smarter-validation

- should I remove Maybe's where I can ([] as Nothing)?

--}

---
-- imports

import           Control.Exception.Base
import           Data.Char
import           Data.Function
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Ord
import           Data.Tree

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

type Index   = Int
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

_dep :: Token -> Dep
_dep t = let (Just (dr,_)) = _deprel t in dr

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
mkDep :: String -> Dep
mkDep = mkDep' . upcaseString
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

mkPos :: String -> Pos
mkPos = mkPos' . upcaseString
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

-- trees
isRoot :: Token -> Bool
isRoot t = ROOT == _dep t

toETree :: Sentence -> ETree
toETree s =
  let (sts, mts) = sentTksByType s
  in (sTksToTTree sts, mts)

sentSTks :: Sentence -> [Token]
sentSTks = fst . sentTksByType

sentTksByType :: Sentence -> ([Token],[Token])
-- ([SToken],[metaTokens:EToken,MToken])
sentTksByType Sentence{_tokens=ts} = partition isSToken ts

isSToken :: Token -> Bool
isSToken SToken{} = True
isSToken _        = False

sTksToTTree :: [Token] -> TTree
sTksToTTree ts =
  let (Just rt) = find isRoot ts
      dm = tksDepMap ts
  in rootToTTree rt dm
  where
    rootToTTree :: Token -> M.Map Index [Token] -> TTree
    rootToTTree t m =
      Node t $
      map (`rootToTTree` m) $ fromMaybe [] $ M.lookup (_ix t) m

tksDepMap :: [Token] -> M.Map Index [Token]
-- Map parent_ix [children_tks]
tksDepMap = foldr mkDepMap M.empty
  where
    mkDepMap t m =
      let hi = fromJust $ _dephead t
          cs = M.lookup hi m
      in if isJust cs
           then M.insertWith (++) hi [t] m
           else M.insert hi [t] m

fromETree :: ETree -> [Token]
--[] sorted list not implemented (use insertBy for meta tokens)
fromETree et =
  let (tt, mts) = et
  in mts ++ flatten tt

drawTTree :: TTree -> String
drawTTree tt = drawTree $ fmap showSToken tt
  where
    showSToken SToken {_ix = ix, _form = (Just fo)} =
      (show ix :: String) ++ "_" ++ fo

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
upcaseString :: String -> String
upcaseString = map toUpper

assNothing :: Maybe a -> Bool -> Bool
assNothing m = assert (isNothing m)

assSomething :: Maybe a -> Bool -> Bool
assSomething m = assert (isJust m)

assNull :: [a] -> Bool -> Bool
assNull l = assert (null l)
