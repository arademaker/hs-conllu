
module Query where

import Conllu.Type

type CField = String
data Query
  = FieldValueQ CField String -- ^  (deprel appos), (form brasil), (misc blablabla), (lemma *)
  | AndQ [Query] -- ^ (and (deprel appos) (form mãe))
  | OrQ [Query] -- ^ (or (id 3) (feats Tense=Past))
  | NegQ Query -- ^ (not (deprel nsubj))
  | HeadQ Query -- ^ (-> (deprel root))


type QueryR = [([[Index]] -- ^ so that we can return singletons,
                          -- pairs, triples, etc of tokens
               , Sentence)]
