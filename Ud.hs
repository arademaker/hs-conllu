module Ud where

data Sentence = Sentence
  { --lif     :: Int -- line in file -- how to obtain this? introspect parsec
  , meta    :: [Comment]
  , tokens  :: [Token]
--  , mtokens :: [Token] -- is this needed?
  } deriving (Eq, Show)

type Comment = (String, String)

-- [ ] add empty token
data Token
  = SToken { id :: Int
           , form :: String
           , lemma :: [String]
           , upostag :: PosTag
           , xpostag :: String
           , feats :: [String]
           , dephead :: Int
           , deprel :: DepRel
           , deps :: [(Int, DepRel)]
           , misc :: String }
  | MToken { start :: Int
           , end :: Int
           , form :: String
           , misc :: String }
  deriving (Eq, Show)

emptyField = "_"

data DepRel
  = Acl
  | Advcl
  | Advmod
  | Amod
  | Appos
  | Aux
  | Case
  | Cc
  | Ccomp
  | Clf
  | Compound
  | Conj
  | Cop
  | Csubj
  | Dep
  | Det
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
  | Punct
  | Reparandum
  | Root
  | Vocative
  | Xcomp
  deriving (Eq, Show)

data PosTag
  = Adj
  | Adp
  | Adv
  | POSAux
  | Cconj
  | POSDet
  | Intj
  | Noun
  | Num
  | Part
  | Pron
  | Propn
  | POSPunct
  | Sconj
  | Sym
  | Verb
  | X
  deriving (Eq, Show)
