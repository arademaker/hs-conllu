-- |
-- Module      :  Conllu.DeprelTagset
-- Copyright   :  © 2018 bruno cuconato
-- License     :  LPGL-3
--
-- Maintainer  :  bruno cuconato <bcclaro+hackage@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- defines the CoNLL-U DEPREL tagset. this module is meant to be imported as
--
-- @
-- import qualified Conllu.DeprelTagset as D
-- @

-- TODO create the UD Taxonomy Table and enhanced dependencies
-- < see https://universaldependencies.org/v2/conll-u.html>
-- TODO include modifier labels, e.g. passive verbs, relative cluses, etc.
-- TODO resolve deriving strategy

module Conllu.DeprelTagset
where  
{-  original declaration:
data EP
  = REF -- ^ only allowed in DEPS
  | ACL
  | ADVCL
  | ADVMOD
  | AMOD
  | APPOS
  | AUX
  | CASE
  | CCOMP
  | CC
  | CLF
  | COMPOUND
  | CONJ
  | COP
  | CSUBJ
  | DEP
  | DET
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
  | PUNCT
  | REPARANDUM
  | ROOT
  | VOCATIVE
  | XCOMP
  deriving ( Show, Eq )  -- , Read,Enum ) It is not clear why Enum and Eq are required 
-}

-- Revised Universal Dependency Relations
-- TODO include subtyped relations and Structural Categories of the dependent
-- Universal Dependencies are represented lowercase
-- see https://universaldependencies.org/u/dep/index.html 

data EP = -- DEPREL  =
        REF        -- ^ only allowed in DEPS
      |  ACL        -- clausal modifier of noun (adnominal clause)
--      | ACL RELCL           -- relative clause modifier
      | ADVCL      -- adverbial clause modifier
      | ADVMOD     -- adverbial modifier
--      | ADVMOD EMPH         -- emphasizing word, intensifier
--      | ADVMOD LMOD         -- locative adverbial modifier
      | AMOD       -- adjectival modifier
      | APPOS      -- appositional modifier
      | AUX        -- auxiliary
--      | AUX PASS   -- passive auxiliary
      | CASE       -- case marking
      | CC         -- coordinating conjunction
--      | CC PRECONJ -- preconjunct
      | CCOMP      -- clausal complement
      | CLF        -- classifier
      | COMPOUND   -- compound
--      | COMPOUND LVC        -- light verb construction
--      | COMPOUND PRT        -- phrasal verb particle
--      | COMPOUND REDUP      -- reduplicated compounds
--      | COMPOUND SVC        -- serial verb compounds
      | CONJ       -- conjunct
      | COP        -- copula
      | CSUBJ      -- clausal subject
 --     | CSUBJ PASS          -- clausal passive subject
      | DEP        -- unspecified dependency
      | DET        -- determiner
--      | DET NUMGOV          -- pronominal quantifier governing the case of the noun
--      | DET NUMMOD          -- pronominal quantifier agreeing in case with the noun
--      | DET POSS            -- possessive determiner
      | DISCOURSE  -- discourse element
      | DISLOCATED -- dislocated elements
      | EXPL       -- expletive
--      | EXPL IMPERS         -- impersonal expletive
--      | EXPL PASS           -- reflexive pronoun used in reflexive passive
--      | EXPL PV             -- reflexive clitic with an inherently reflexive verb
      | FIXED      -- fixed multiword expression
      | FLAT       -- flat multiword expression
--      | FLAT FOREIGN        -- foreign words
--      | FLAT NAME           -- names
      | GOESWITH   -- goes with
      | IOBJ       -- indirect object
      | LIST       -- list
      | MARK       -- marker
      | NMOD       -- nominal modifier
--      | NMOD POSS           -- possessive nominal modifier
--      | NMOD TMOD           -- temporal modifier
      | NSUBJ      -- nominal subject
 --     | NSUBJ PASS          -- passive nominal subject
      | NUMMOD     -- numeric modifier
 --     | NUMMOD GOV          -- numeric modifier governing the case of the noun
      | OBJ        -- object
      | OBL        -- oblique nominal
--      | OBL AGENT           -- agent modifier
--      | OBL ARG             -- oblique argument
--      | OBL LMOD            -- locative modifier
--      | OBL TMOD            -- temporal modifier
      | ORPHAN     -- orphan
      | PARATAXIS  -- parataxis
      | PUNCT      -- punctuation
      | REPARANDUM -- overridden disfluency
      | ROOT       -- root
      | VOCATIVE   -- vocative
      | XCOMP      -- open clausal complement
      deriving ( Show, Eq )
      
      
  --  Structural Categories of the dependent
{-
type StructuralCats = [Nominals , Clauses , ModifierWords , FunctionWords]
type Nominals       = [Nsubj , Obj , Iobj , Obl , Vocative , Expl , Dislocated , Nmod , Appos , Nummod]
type Clauses        = [Csubj , Ccomp , Xcomp , Advcl , Acl]
type ModifierWords  = [Advmod , Discourse , Amod]
type FunctionWords  = [Aux , Cop , Mark , Det , Clf , Case]

-- Functional Categories in relation to the head
type FunctionalCats   = [ Core , NonCore , NominalDependent]
type Core             = [Nsubj , Obj , Iobj , Csubj , Ccomp , Xcomp]
type NonCore          = [Obl , Vocative , Expl , Dislocated , Advcl , Advmod , Discourse , Aux , Cop , Mark]
type NominalDependent = [Nmod , Appos , Nummod , Acl , Amod , Det , Clf , Case]

-- Relations that are not dependency relations in the narrow sense.
type Coordination     = [ Conj , Cc]
type MWE              = [ Fixed , Flat , Compound] -- MultiWordExpressions
type Loose            = [ List , Parataxis]
type Special          = [ Orphan , Goeswith , Reparandum]
type Other            = [ Punct , Root , Dep]
-}


