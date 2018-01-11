module UD where

{--

- TODO: validate input of mk*Token functions https://github.com/tonymorris/validation, https://github.com/mavenraven/validations, https://ro-che.info/articles/2015-05-02-smarter-validation

- should I remove Maybe's where I can ([] as Nothing)?

--}

---
-- imports
-- stdlib
import Data.Char
import Data.Maybe
import Data.Ord
import Data.List
import Data.Tree

---
-- type and data declarations
data Document = Document
  { file      :: String
  , sents     :: [Sentence]
  } deriving (Eq,Show)

data Sentence = Sentence
  { --lif     :: Int, -- line in file -- how to obtain this? introspect parsec
   meta      :: [Comment]
  , tokens    :: [Token]
  } deriving (Eq, Show)

type Comment    = StringPair
type StringPair = (String, String)

data Token
  = SToken { ix      :: Index
           , form    :: Form
           , lemma   :: Lemma
           , upostag :: PosTag
           , xpostag :: Xpostag
           , feats   :: Feats
           , dephead :: Dephead
           , deprel  :: DepRel
           , deps    :: Deps
           , misc    :: Misc
           }
  | MToken { start :: Index
           , end   :: Index
           , form  :: Form
           , misc  :: Misc
           -- other values should be empty
           }
  | EToken { ix       :: Index
           , childIx  :: Index
           , form     :: Form
           , lemma    :: Lemma
           , upostag  :: PosTag
           , xpostag  :: Xpostag
           , feats    :: Feats
           -- heads and deprels specified in deps
           , deps     :: Deps
           , misc     :: Misc
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

dep :: Token -> Dep
dep t = let (Just (dr,_)) = deprel t in dr

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
  SToken { ix      = i
         , form    = fo
         , lemma   = l
         , upostag = up
         , xpostag = xp
         , feats   = fe
         , dephead = h
         , deprel  = dr
         , deps    = d
         , misc    = m
         }

mkMToken :: Index ->  Index -> Form -> Lemma -> PosTag -> Xpostag
  -> Feats -> Dephead -> DepRel -> Deps -> Misc -> Token
mkMToken s e fo l up xp fe h dr d m =
  MToken {start = s, end = e, form = fo, misc = m}

mkEToken :: Index ->  Index -> Form -> Lemma -> PosTag -> Xpostag
  -> Feats -> Dephead -> DepRel -> Deps -> Misc -> Token
mkEToken i ci fo l up xp fe h dr d m =
  EToken
  { ix       = i
  , childIx  = ci
  , form    = fo
  , lemma   = l
  , upostag = up
  , xpostag  = xp
  , feats    = fe
  , deps     = d
  , misc     = m
  }

-- trees
toETree :: Sentence -> ETree
toETree s =
  let (sts, mts) = partition isSToken $ tokens s
  in (tokensToTTree $ sortByDepRel sts, mts)

isSToken :: Token -> Bool
isSToken SToken{} = True
isSToken _        = False

tokensToTTree :: [Token] -> TTree
tokensToTTree (t:tt) = foldl' addToken (Node t []) tt

addToken :: TTree -> Token -> TTree
addToken (Node p cs) t@(SToken{dephead = (Just dh)}) =
  if dh == ix p
    then Node p $ (Node t []):cs
    else Node p $ fmap (\c -> addToken c t) cs

sortByDepRel :: [Token] -> [Token]
sortByDepRel = sortBy sortByDepRel'
  where
    sortByDepRel' SToken{dephead=(Just i1)} SToken{dephead=(Just i2)} = compare i1 i2
    sortByDepRel' _ _ = EQ

fromETree :: ETree -> [Token]
--[] sorted list not implemented (use insertBy for meta tokens)
fromETree et =
  let (tt, mts) = et
  in mts ++ flatten tt

drawTTree :: TTree -> String
drawTTree tt = drawTree $ fmap showSToken tt
  where showSToken t = case t of
          (SToken{ix=ix,form=(Just fo)}) -> (show ix :: String) ++ "_" ++ fo
          EToken{} -> "ROOT"

---
-- utility functions
upcaseString :: String -> String
upcaseString = map toUpper
