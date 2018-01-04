module Ud where

data Sentence = Sentence
  { --lif     :: Int -- line in file -- how to obtain this? introspect parsec
  , meta      :: [Comment]
  , tokens    :: [Token]
--  , mtokens :: [Token] -- is this needed?
  } deriving (Eq, Show)

type Comment = (String, String)

-- [ ] add empty token
data Token
  = SToken { id      :: Tid
           , form    :: Form
           , lemma   :: Lemma
           , upostag :: PosTag
           , xpostag :: String
           , feats   :: Feats
           , dephead :: Dephead
           , deprel  :: DepRel
           , deps    :: Deps
           , misc    :: Misc }
  | MToken { start :: Tstart
           , end   :: Tend
           , form  :: Form
           , misc  :: Misc }
  deriving (Eq, Show)

type Tid      = Int
type Form    = String
type Lemma   = [String]
type Feats   = [(String, String)]
type Dephead = Int
type Deps    = [(Int, DepRel)]
type Misc    = String
type Tstart  = Int
type Tend    = Int

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

mkPosTag :: String -> PosTag
mkPosTag "Adj"      = Adj
mkPosTag "Adp"      = Adp
mkPosTag "Adv"      = Adv
mkPosTag "AuxPOs"   = AuxPOS
mkPosTag "Cconj"    = Cconj
mkPosTag "DetPOS"   = DetPOS
mkPosTag "Intj"     = Intj
mkPosTag "Noun"     = Noun
mkPosTag "Num"      = Num
mkPosTag "Part"     = Part
mkPosTag "Pron"     = Pron
mkPosTag "Propn"    = Propn
mkPosTag "PunctPOS" = PunctPOS
mkPosTag "Sconj"    = Sconj
mkPosTag "Sym"      = Sym
mkPosTag "Verb"     = Verb
mkPosTag "X"        = X
