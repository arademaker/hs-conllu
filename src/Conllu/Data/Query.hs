
module Query where

import Conllu.Type



type CField = String

data CQuery = CQuery [Query] [TQuery]

data TQuery = HeadQ Query -- ^ (-> (deprel root))
  | PrecedQ [Index]
  | EqualQ [Query]
  deriving (Eq,Show)

data Query
  = FieldValueQ CField String -- ^  (deprel appos), (form brasil), (misc blablabla), (lemma *)
  | AndQ [Query] -- ^ (and (deprel appos) (form mãe))
  | OrQ [Query] -- ^ (or (id 3) (feats Tense=Past))
  | NegQ Query -- ^ (not (deprel nsubj))
  deriving (Eq,Show)


type QueryR = [([[Index]] -- ^ so that we can return singletons,
                          -- pairs, triples, etc of tokens
               , Sentence)]
