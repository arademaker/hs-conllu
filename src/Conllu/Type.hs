-- |
-- Module      :  Conllu.Type
-- Copyright   :  © 2018 bruno cuconato
-- License     :  LPGL-3
--
-- Maintainer  :  bruno cuconato <bcclaro+hackage@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- defines types for handling CoNLL-U data.

{-# LANGUAGE EmptyDataDecls #-}

module Conllu.Type where

---
-- imports
import           Conllu.Utils
import qualified Conllu.UposTagset as U
import qualified Conllu.DeprelTagset as D
import           Data.Text
import           Data.Ord (comparing)


---
-- * type and data declarations
-- ** Documents and Sentences
type Doc = [Sent]
type Par = [Sent]

{- | Sentence Boundaries
      There must be exactly one blank line after every sentence, including the last sentence in the file.
      Empty sentences are not allowed.
      Lines starting with the # character and preceding a sentence are considered as carrying comments or metadata
      relevant to the following sentence.
            TODO add a command line option to ignore comments and or metadata
      The contents of the comments and metadata is basically unrestricted, may be application specific
            TODO describe how application specific comment and metadata parsing can be accomplished
      Two comments are compulsory
      1) A unique sentence id (# sent_id = ). The actual identifier does not contain whitespace characters
         (while the comment line may contain whitespace around the sent_id keyword and the equals-to sign)
         In sentence ids, the slash character (“/”) is reserved for specialized downstream use.
      2) The unannotated sentence as a single string (# text = ...), (# translit = ...) for transliterations and
         # text_en = ....
-}

data Sent = Sent
  {
   _meta  :: [Comment]  -- ^ the sentence's comments.
  ,
   _words :: [CW AW]    -- ^ the sentence's words.
  } -- deriving (Show) -- , Eq)

-- | Comments can be (key, value) pairs. #\s[A-Za-z0-9-_]+\s\=\s(.)*$
type Comment    = StringPair
type StringPair = (Text, Text)  -- TODO perhaps a Data.Map (Key,Text)

-- ** Words
-- | represents a word line in a CoNLL-U file. note that we have
-- collapsed some fields together: 'HEAD' and DEPREL have been
-- combined as a relation type Rel accessible by the '_rel' function;
-- the 'DEPS' field is merely a list of 'Rel'.
--
--   a C(oNLL-U)W(ord) may be a simple word, a multi-word token, or an
--   empty node. this is captured by the phantom type (the `a` in the
--   declaration), which can be parametrized by one of the data types
--   below in order to build functions that only operate on one of these
--   word types (see 'mkSWord' on how to do this). see the '_dep'
--   function, which only operates on simple words, which are the ones
--   that have a DEPREL field.
{-  TODO In the Parse Module there is an equivalent data type for ParserC
       perhaps this can be done as a transformation CW -> Parser CW -}
data CW a = CW
  { _id    :: ID        -- ^ ID field
  , _form  :: FORM      -- ^ FORM field
  , _lemma :: LEMMA     -- ^ LEMMA field
  , _upos  :: UPOS      -- ^ UPOS field
  , _xpos  :: XPOS      -- ^ XPOS field
  , _feats :: FEATS     -- ^ FEATS field
  , _rel   :: Maybe Rel -- ^ combined HEAD and DEPREL fields
  , _deps  :: DEPS      -- ^ DEPS field
  , _misc  :: MISC      -- ^ MISC field
  } deriving (Show, Eq)

-- | To ensure token ids follow the order described in the Word, Tokens and Empty Nodes
--   the Ord class is used for totally ordered datatypes
--   Conllu Words, Tokens and Empty Nodes <https://universaldependencies.org/format.html#words-tokens-and-empty-nodes>

instance Ord (CW a) where
  compare = comparing _id

-- *** Word types
-- | phantom type for any kind of word.
data AW -- anyWord
-- | phantom type for a simple word.
data SW -- simpleWord
-- | phantom type for multiword tokens. do note that in MWTs only the
-- 'ID', 'FORM' and 'MISC' fields may be non-empty.
data MT --multiWord
-- | phantom type for an empty node. i.e. an inferred word not actually in sentence,
--   i.e. Sue likes coffee and Bill (likes) tea, where (likes) is an empty node.
data EN -- Empty Node

-- *** Word Fields
data ID -- | Word ID field.  -- TODO tuple?? (Index,Index)
  = SID Index -- ^ word ID is an integer
  | MID Index
        Index -- ^ multi-word token ID is a range, which may not overlap TODO [Index ... Index]
  | EID Index
        Index -- ^ empty node ID is a decimal, where 5.9 < 5.10
  deriving (Eq, Show)

type Index   = Int


instance Ord ID where
  compare = idOrd
    where
      idOrd :: ID -> ID -> Ordering
      idOrd (SID x) (SID y) = compare x y  -- x and y are Int
      idOrd id1 id2 =
        let c = comparing fstIx id1 id2
        in case c of
          EQ -> sameIx id1 id2
          _ -> c
        where
          fstIx :: ID -> Index  -- Convert an ID into an Index
          fstIx (SID i) = i     -- SID is an Int
          fstIx (MID i _ei) = i -- MID is a range, e.g. 3-4
          fstIx (EID i _ei) = i -- EID is a decimal > 0 . e.g. 0.2,
          sndIx :: ID -> Index
          sndIx (EID _s e) = e
          sndIx (MID _s e) = e
          sndIx (SID i ) = i -- there is no sndIX
          sameIx :: ID -> ID -> Ordering
          sameIx (SID _) _id = GT
          sameIx _id (SID _) = LT
          -- reverse ID order so that MID 1 4 comes before MID 1 2:
          sameIx i1 i2 = comparing sndIx i2 i1

type FORM  = Maybe Text  -- ^ Word form or punctuation
type LEMMA = Maybe Text  -- ^ Lemma or Stem Word
type UPOS  = Maybe U.POS -- ^ Universal Part of Speech
type XPOS  = Maybe Text  -- ^ language specific part of speech
type FEATS = [Feat]      -- ^ language specific features in data.feats.json
type HEAD  = ID          -- ^ Head of the current word -- tree structure -- root = 0 catena order
type DEPS  = [Rel]       -- ^ depRel Universal Dependency Relation or language specific subset
type MISC  = Maybe Text  -- ^ Any other information

-- | feature representation
--   Todo evaluate against the Universal Dependency python Verify.FeatureSet
data Feat = Feat
  { _feat       :: Text       -- ^ feature name
  , _featValues :: [Text]     -- ^ feature values
  , _featType   :: Maybe Text -- ^ feature type (inside brackets).
  } deriving (Show, Eq )   -- add manual Ord instance?

-- | dependency relation representation.
data Rel = Rel
  { _head :: HEAD         -- ^ head 'ID'
  , _deprel :: D.EP       -- ^ dependency relation type
  , _subdep :: Maybe Text -- ^ dependency relation subtype
  , _rest :: Maybe [Text] -- ^ provisitonal, see issues #23,#17
  }  deriving (Show, Eq)  -- Needed for Data CW definition



---
-- ** accessor functions
_dep :: CW SW -> Maybe D.EP
-- | get DEPREL main value, if it exists.
_dep w = Just . _deprel =<< _rel w

depIs :: D.EP -> CW SW -> Bool
-- | check if DEP is the one provided.
depIs d = (Just d ==) . _dep

--- The following are no longer required:
{-
-- ** constructor functions
mkDEP ::  Text -> Maybe D.EP
-- | read a main DEPREL (no subtype).
mkDEP = readMaybe . toString . toUppoer -- upcaseStr'

mkUPOS :: Text -> Maybe U.POS
-- | read an 'UPOS' tag.
mkUPOS =  readMaybe . toString . toUpper -- upcaseStr'
-}

{--
-- words
mkAW :: ID -> FORM -> LEMMA -> UPOS -> XPOS -> FEATS -> Maybe Rel
  -> DEPS -> MISC -> CW AW
-- | make a word from its fields, by default it has phantom type of AW
-- (any kind of word).
mkAW = CW

mkSW :: CW AW -> CW SW
-- | coerce a word to a simple word.
mkSW CW { _id = i
        , _form = f
        , _lemma = l
        , _upos = u
        , _xpos = x
        , _feats = fs
        , _rel = r
        , _deps = ds
        , _misc = m
 = CW i f l u x fs r ds m
--}
{-- saved for a future validation module
---
-- validation
mTkOK :: FORM -> LEMMA -> UPOS -> XPOS -> FEATS -> (ID, (Dep, Maybe Text))
  -> Bool
mTkOK fo l up xp fe h dr d =
  assSomething fo $
  assNothing l $
  assNothing up $
  assNothing xp $
  assNull fe $ assNothing h $ assNothing dr $ assNull d True

eTkOK :: Dephead -> DepRel -> Deps -> Bool
eTkOK h dr d =
  assNothing h $ assNothing dr $ (assert . not . null $ d) True
--}