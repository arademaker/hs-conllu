module Ud where

{--

- TODO: validate input of mk*Token functions https://github.com/tonymorris/validation, https://github.com/mavenraven/validations, https://ro-che.info/articles/2015-05-02-smarter-validation

--}

---
-- imports

-- stdlib
import Data.Char
import Data.Maybe

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
           -- no head and deprel -- same as id token
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
type Deps    = [(Index, DepRel)]
type Misc    = Maybe String

data DepRel
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

---
-- constructor functions
mkDepRel :: String -> DepRel
mkDepRel = mkDepRel' . upcaseString
  where
    mkDepRel' "ACL"        = ACL
    mkDepRel' "ADVCL"      = ADVCL
    mkDepRel' "ADVMOD"     = ADVMOD
    mkDepRel' "AMOD"       = AMOD
    mkDepRel' "APPOS"      = APPOS
    mkDepRel' "AUX"        = AUXdr
    mkDepRel' "CASE"       = CASE
    mkDepRel' "CC"         = CC
    mkDepRel' "CCOMP"      = CCOMP
    mkDepRel' "CLF"        = CLF
    mkDepRel' "COMPOUND"   = COMPOUND
    mkDepRel' "CONJ"       = CONJ
    mkDepRel' "COP"        = COP
    mkDepRel' "CSUBJ"      = CSUBJ
    mkDepRel' "DEP"        = DEP
    mkDepRel' "DET"        = DETdr
    mkDepRel' "DISCOURSE"  = DISCOURSE
    mkDepRel' "DISLOCATED" = DISLOCATED
    mkDepRel' "EXPL"       = EXPL
    mkDepRel' "FIXED"      = FIXED
    mkDepRel' "FLAT"       = FLAT
    mkDepRel' "GOESWITH"   = GOESWITH
    mkDepRel' "IOBJ"       = IOBJ
    mkDepRel' "LIST"       = LIST
    mkDepRel' "MARK"       = MARK
    mkDepRel' "NMOD"       = NMOD
    mkDepRel' "NSUBJ"      = NSUBJ
    mkDepRel' "NUMMOD"     = NUMMOD
    mkDepRel' "OBJ"        = OBJ
    mkDepRel' "OBL"        = OBL
    mkDepRel' "ORPHAN"     = ORPHAN
    mkDepRel' "PARATAXIS"  = PARATAXIS
    mkDepRel' "PUNCT"      = PUNCTdr
    mkDepRel' "REPARANDUM" = REPARANDUM
    mkDepRel' "ROOT"       = ROOT
    mkDepRel' "VOCATIVE"   = VOCATIVE
    mkDepRel' "XCOMP"      = XCOMP

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

---
-- utility functions
upcaseString :: String -> String
upcaseString = map toUpper
