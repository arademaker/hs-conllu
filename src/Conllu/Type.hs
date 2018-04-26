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

import           Data.Ord

---
-- * type and data declarations
-- ** Documents and Sentences
type Doc = [Sent]

data Sent = Sent
  { _meta  :: [Comment]  -- ^ the sentence's comments.
  , _words :: [CW AW] -- ^ the sentence's words.
  } deriving (Eq, Show)

-- | most comments are (key, value) pairs.
type Comment    = StringPair
type StringPair = (String, String)

-- ** Words
-- | represents a word line in a CoNLL-U file. note that we have
-- collapsed some fields together: 'HEAD' and DEPREL have been
-- combined as a relation type Rel accessible by the '_rel' function;
-- the 'DEPS' field is merely a list of 'Rel'.
--
-- a C(oNLL-U)W(ord) may be a simple word, a multi-word token, or an
-- empty node. this is captured by the phantom type (the `a` in the
-- declaration), which can be parametrized by one of the data types
-- below in order to build functions that only operate on one of these
-- word types (see 'mkSWord' on how to do this). see the '_dep'
-- function, which only operates on simple words, which are the ones
-- that have a DEPREL field.
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
  } deriving (Eq, Show)

instance Ord (CW a) where
  compare = comparing _id

-- *** Word types
-- | phantom type for any kind of word.
data AW
-- | phantom type for a simple word.
data SW
-- | phantom type for multiword tokens. do note that in MWTs only the
-- 'ID', 'FORM' and 'MISC' fields may be non-empty.
data MT
-- | phantom type for an empty node.
data EN

-- *** Word Fields
data ID -- | Word ID field.
  = SID Index -- ^ word ID is an integer
  | MID Index
        Index -- ^ multi-word token ID is a range
  | EID Index
        Index -- ^ empty node ID is a decimal
  deriving (Eq, Show)

instance Ord ID where
  compare = idOrd
    where
      idOrd :: ID -> ID -> Ordering
      idOrd id1 id2 =
        let c = comparing fstIx id1 id2
        in case c of
          EQ -> sameIx id1 id2
          _ -> c
        where
          fstIx :: ID -> Index
          fstIx (SID i) = i
          fstIx (MID i _ei) = i
          fstIx (EID i _ei) = i
          sndIx :: ID -> Index
          sndIx (EID _s e) = e
          sndIx (MID _s e) = e
          sameIx :: ID -> ID -> Ordering
          sameIx (SID _) _id = GT
          sameIx _id (SID _) = LT
          -- reverse ID order so that MID 1 4 comes before MID 1 2:
          sameIx i1 i2 = comparing sndIx i2 i1

type FORM  = Maybe String
type LEMMA = Maybe String
type UPOS  = Maybe U.POS
type XPOS  = Maybe String
type FEATS = [Feat]
type HEAD  = ID
type DEPS  = [Rel]
type MISC  = Maybe String

-- | feature representation
data Feat = Feat
  { _feat       :: String       -- ^ feature name
  , _featValues :: [String]     -- ^ feature values
  , _featType   :: Maybe String -- ^ feature type (inside brackets).
  } deriving (Eq, Show) -- add manual Ord instance?

-- | dependency relation representation.
data Rel = Rel
  { _head   :: HEAD         -- ^ head 'ID'
  , _deprel :: D.EP         -- ^ dependency relation type
  , _subdep :: Maybe String -- ^ dependency relation subtype
  } deriving (Eq, Show)

type Index   = Int
-- | 'ID' separator in meta words
type IxSep   = Char

---
-- ** accessor functions
_dep :: CW SW -> Maybe D.EP
-- | get DEPREL main value, if it exists.
_dep w = Just . _deprel =<< _rel w

depIs :: D.EP -> CW SW -> Bool
-- | check if DEP is the one provided.
depIs d = maybe False (\d' -> d == d') . _dep

---
-- ** constructor functions
mkDEP :: String -> D.EP
-- | read a main DEPREL (no subtype).
mkDEP = read . upcaseStr

mkUPOS :: String -> U.POS
-- | read an 'UPOS' tag.
mkUPOS = read . upcaseStr

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
        } = CW i f l u x fs r ds m

{-- saved for a future validation module
---
-- validation
mTkOK :: FORM -> LEMMA -> UPOS -> XPOS -> FEATS -> (ID, (Dep, Maybe String))
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
