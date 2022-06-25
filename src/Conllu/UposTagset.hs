-- |
-- Module      :  Conllu.UposTagset
-- Copyright   :  © 2018 bruno cuconato
-- License     :  LPGL-3
--
-- Maintainer  :  bruno cuconato <bcclaro+hackage@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- defines the CoNLL-U UPOS tagset. this module is meant to be imported as
--
-- @
-- import qualified Conllu.UposTagset as U
-- @

module Conllu.UposTagset where  

data POS
  = ADJ   --  adjective
  | ADP   --  adposition
  | ADV   --  adverb
  | AUX   --  auxiliary
  | CCONJ --  coordinating conjunction
  | DET   --  determiner
  | INTJ  --  interjection
  | NOUN  --  noun
  | NUM   --  numeral
  | PART  --  particle
  | PRON  --  pronoun
  | PROPN --  proper noun
  | PUNCT --  punctuation
  | SCONJ --  subordinating conjunction
  | SYM   --  symbol
  | VERB  --  verb
  | X     --  other
 deriving (Enum, Eq, Read, Show ) 
