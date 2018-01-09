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
           , eForm    :: Maybe Form
           , eLemma   :: Maybe Lemma
           , eUpostag :: Maybe PosTag
           , xpostag  :: Xpostag
           , feats    :: Feats
           -- heads and deprels specified in deps
           , deps     :: Deps
           , misc     :: Misc
           -- field with prefix 'e' represents the fact that it is
           -- different from the one in SToken (can be empty in EToken
           -- but not in SToken, or vice-versa)
           }
  deriving (Eq, Show)

type Index   = Int
type IxSep   = Char
type Form    = String
type Lemma   = String
type Xpostag = Maybe String
type Feats   = [StringPair]
type Dephead = Index
type DepRel  = (Dep,String)
type Deps    = [(Index, DepRel)]
type Misc    = Maybe String

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

data PosTag
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

type TTree  = Tree Token -- only STokens

root :: TTree
root = Node (EToken 0 0 Nothing Nothing Nothing Nothing [] [] Nothing) []

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

mkPosTag :: String -> PosTag
mkPosTag = mkPosTag' . upcaseString
  where
    mkPosTag' "ADJ"      = ADJ
    mkPosTag' "ADP"      = ADP
    mkPosTag' "ADV"      = ADV
    mkPosTag' "AUX"      = AUXpos
    mkPosTag' "CCONJ"    = CCONJ
    mkPosTag' "DET"      = DETpos
    mkPosTag' "INTJ"     = INTJ
    mkPosTag' "NOUN"     = NOUN
    mkPosTag' "NUM"      = NUM
    mkPosTag' "PART"     = PART
    mkPosTag' "PRON"     = PRON
    mkPosTag' "PROPN"    = PROPN
    mkPosTag' "PUNCT"    = PUNCTpos
    mkPosTag' "SCONJ"    = SCONJ
    mkPosTag' "SYM"      = SYM
    mkPosTag' "VERB"     = VERB
    mkPosTag' "X"        = X

mkToken :: Index -> Maybe IxSep -> Maybe Index -> Maybe Form
  -> Maybe Lemma -> Maybe PosTag -> Xpostag -> Feats -> Maybe Dephead
  -> Maybe DepRel -> Deps -> Misc -> Token
mkToken ix sep = case sep of
  Nothing  -> mkSToken ix
  Just '-' -> mkMToken ix
  Just '.' -> mkEToken ix

mkSToken :: Index -> Maybe Index -> Maybe Form -> Maybe Lemma
  -> Maybe PosTag -> Xpostag -> Feats -> Maybe Dephead -> Maybe DepRel
  -> Deps -> Misc -> Token
mkSToken i ci fo l up xp fe h dr d m =
  SToken { ix      = i
         , form    = fromJust fo
         , lemma   = fromJust l
         , upostag = fromJust up
         , xpostag = xp
         , feats   = fe
         , dephead = fromJust h
         , deprel  = fromJust dr
         , deps    = d
         , misc    = m
         }

mkMToken :: Index -> Maybe Index -> Maybe Form -> Maybe Lemma
  -> Maybe PosTag -> Xpostag -> Feats -> Maybe Dephead -> Maybe DepRel
  -> Deps -> Misc -> Token
mkMToken s e fo l up xp fe h dr d m =
  MToken {start = s, end = fromJust e, form = fromJust fo, misc = m}

mkEToken :: Index -> Maybe Index -> Maybe Form -> Maybe Lemma
  -> Maybe PosTag -> Xpostag -> Feats -> Maybe Dephead -> Maybe DepRel
  -> Deps -> Misc -> Token
mkEToken i ci fo l up xp fe h dr d m =
  EToken
  { ix       = i
  , childIx  = fromJust ci
  , eForm    = fo
  , eLemma   = l
  , eUpostag = up
  , xpostag  = xp
  , feats    = fe
  , deps     = d
  , misc     = m
  }

toETree :: Sentence -> ETree
toETree s =
  let (sts, mts) = partition isSToken $ tokens s
  in (tokensToTTree $ sortByDepRel sts, mts)

isSToken :: Token -> Bool
isSToken SToken{} = True
isSToken _        = False

tokensToTTree :: [Token] -> TTree
tokensToTTree ts = foldl' addToken root ts

addToken :: TTree -> Token -> TTree
addToken (Node p cs) t@(SToken{dephead = dh}) =
  if dh == ix p
    then Node p $ (Node t []):cs
    else Node p $ fmap (\c -> addToken c t) cs

sortByDepRel :: [Token] -> [Token]
sortByDepRel = sortBy sortByDepRel'
  where
    sortByDepRel' SToken{dephead=i1} SToken{dephead=i2} = compare i1 i2
    sortByDepRel' _ _ = EQ

fromETree :: ETree -> [Token]
--[] sorted list not implemented (use insertBy for meta tokens)
fromETree et =
  let (tt, mts) = et
  in mts ++ flatten tt

drawTTree :: TTree -> String
drawTTree tt = drawTree $ fmap showSToken tt
  where showSToken t = case t of
          (SToken{ix=ix,form=fo}) -> (show ix :: String) ++ "_" ++ fo
          EToken{} -> "ROOT"

---
-- utility functions
upcaseString :: String -> String
upcaseString = map toUpper
