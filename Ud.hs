module Ud where

{--

- TODO: validate input of mk*Token functions https://github.com/tonymorris/validation, https://github.com/mavenraven/validations, https://ro-che.info/articles/2015-05-02-smarter-validation

--}

---
-- imports

-- stdlib
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
           , deps    :: Maybe Deps
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
           , eDeps    :: Deps
           , misc     :: Misc
           -- field with prefix 'e' represents the fact that it is
           -- different from the one in SToken (can be empty in EToken
           -- but not in SToken, or vice-versa)
           }
  deriving (Eq, Show)

type Index   = Int
type IxSep   = Maybe Char
type Form    = String
type Lemma   = String
type Xpostag = Maybe String
type Feats   = [StringPair]
type Dephead = Index
type Deps    = [(Index, DepRel)]
type Misc    = Maybe String

data DepRel
  = Acl
  | Advcl
  | Advmod
  | Amod
  | Appos
  | AuxDR
  | Case
  | Cc
  | Ccomp
  | Clf
  | Compound
  | Conj
  | Cop
  | Csubj
  | Dep
  | DetDR
  | Discourse
  | Dislocated
  | Expl
  | Fixed
  | Flat
  | Goeswith
  | Iobj
  | List
  | Mark
  | Nmod
  | Nsubj
  | Nummod
  | Obj
  | Obl
  | Orphan
  | Parataxis
  | PunctDR
  | Reparandum
  | Root
  | Vocative
  | Xcomp
  deriving (Eq, Show)

data PosTag
  = Adj
  | Adp
  | Adv
  | AuxPOS -- POS because there is a Aux in deprel
  | Cconj
  | DetPOS
  | Intj
  | Noun
  | Num
  | Part
  | Pron
  | Propn
  | PunctPOS
  | Sconj
  | Sym
  | Verb
  | X
  deriving (Eq, Show)

---
-- constructor functions
mkSentence :: [Comment] -> [Token] -> Sentence
mkSentence cs ts = Sentence { meta = cs, tokens = ts }

mkDepRel :: String -> DepRel
mkDepRel "Acl"        = Acl
mkDepRel "Advcl"      = Advcl
mkDepRel "Advmod"     = Advmod
mkDepRel "Amod"       = Amod
mkDepRel "Appos"      = Appos
mkDepRel "Aux"        = AuxDR
mkDepRel "Case"       = Case
mkDepRel "Cc"         = Cc
mkDepRel "Ccomp"      = Ccomp
mkDepRel "Clf"        = Clf
mkDepRel "Compound"   = Compound
mkDepRel "Conj"       = Conj
mkDepRel "Cop"        = Cop
mkDepRel "Csubj"      = Csubj
mkDepRel "Dep"        = Dep
mkDepRel "Det"        = DetDR
mkDepRel "Discourse"  = Discourse
mkDepRel "Dislocated" = Dislocated
mkDepRel "Expl"       = Expl
mkDepRel "Fixed"      = Fixed
mkDepRel "Flat"       = Flat
mkDepRel "Goeswith"   = Goeswith
mkDepRel "Iobj"       = Iobj
mkDepRel "List"       = List
mkDepRel "Mark"       = Mark
mkDepRel "Nmod"       = Nmod
mkDepRel "Nsubj"      = Nsubj
mkDepRel "Nummod"     = Nummod
mkDepRel "Obj"        = Obj
mkDepRel "Obl"        = Obl
mkDepRel "Orphan"     = Orphan
mkDepRel "Parataxis"  = Parataxis
mkDepRel "Punct"      = PunctDR
mkDepRel "Reparandum" = Reparandum
mkDepRel "Root"       = Root
mkDepRel "Vocative"   = Vocative
mkDepRel "Xcomp"      = Xcomp

mkPosTag :: String -> PosTag
mkPosTag "Adj"      = Adj
mkPosTag "Adp"      = Adp
mkPosTag "Adv"      = Adv
mkPosTag "Aux"      = AuxPOS
mkPosTag "Cconj"    = Cconj
mkPosTag "Det"      = DetPOS
mkPosTag "Intj"     = Intj
mkPosTag "Noun"     = Noun
mkPosTag "Num"      = Num
mkPosTag "Part"     = Part
mkPosTag "Pron"     = Pron
mkPosTag "Propn"    = Propn
mkPosTag "Punct"    = PunctPOS
mkPosTag "Sconj"    = Sconj
mkPosTag "Sym"      = Sym
mkPosTag "Verb"     = Verb
mkPosTag "X"        = X

mkToken :: Index -> IxSep -> Maybe Index -> Maybe Form
  -> Maybe Lemma -> Maybe PosTag -> Xpostag -> Feats
  -> Dephead -> DepRel -> Maybe Deps -> Misc -> Token
mkToken ix sep = case sep of
  Nothing  -> mkSToken ix
  Just '-' -> mkMToken ix
  Just '.' -> mkEToken ix

mkSToken :: Index -> Maybe Index -> Maybe Form -> Maybe Lemma
  -> Maybe PosTag -> Xpostag -> Feats -> Dephead -> DepRel -> Maybe Deps
  -> Misc -> Token
mkSToken i ci fo l up xp fe h dr d m =
  SToken { ix      = i
         , form    = fromJust fo
         , lemma   = fromJust l
         , upostag = fromJust up
         , xpostag = xp
         , feats   = fe
         , dephead = h
         , deprel  = dr
         , deps    = d
         , misc    = m
         }

mkMToken :: Index -> Maybe Index -> Maybe Form -> Maybe Lemma
  -> Maybe PosTag -> Xpostag -> Feats -> Dephead -> DepRel -> Maybe Deps
  -> Misc -> Token
mkMToken s e fo l up xp fe h dr d m =
  MToken {start = s, end = fromJust e, form = fromJust fo, misc = m}

mkEToken :: Index -> Maybe Index -> Maybe Form -> Maybe Lemma
  -> Maybe PosTag -> Xpostag -> Feats -> Dephead -> DepRel -> Maybe Deps
  -> Misc -> Token
mkEToken i ci fo l up xp fe h dr d m =
  EToken
  { ix       = i
  , childIx  = fromJust ci
  , eForm    = fo
  , eLemma   = l
  , eUpostag = up
  , xpostag  = xp
  , feats    = fe
  , eDeps    = fromJust d
  , misc     = m
  }
