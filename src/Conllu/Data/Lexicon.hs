module Conllu.Data.Lexicon where

---
-- imports
import           Conllu.Type

import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           System.Environment

-- TODO: generalize types
-- TODO: use foldable instance

---
-- types
data Trie a = Trie Bool (M.Map a (Trie a))
  deriving (Show)

type TTrie = Trie String

data InTrie = No | Partially | Yes
  deriving (Eq, Show)

---
-- trie
emptyMap :: M.Map k v
emptyMap = M.empty

emptyTTrie :: TTrie
emptyTTrie = Trie False M.empty

isEmptyTTrie :: TTrie -> Bool
-- simply null if foldable instance
isEmptyTTrie (Trie _ m) = M.null m

insertLex :: TTrie -> [String] -> TTrie
insertLex (Trie _ m) [] = Trie True m
insertLex tt@(Trie v m) (x:xt) =
  let tt' = M.findWithDefault emptyTTrie x m
  in Trie v $ M.insert x (insertLex tt' xt) m

memberLex :: TTrie -> [String] -> InTrie
-- terrible code
memberLex (Trie v _) [] = if v then Yes else Partially
memberLex (Trie _ m) (x:xt) =
  let mt = M.lookup x m
  in case mt of
    Nothing -> No
    Just t -> memberLex t xt

---
-- recognizing
recTks :: TTrie -> [Token] -> [Token]
recTks tt (tk:tks) = case memberLex tt [fromJust $ _form tk] of
  No -> recTks tt tks
  Partially -> tk:(recTks tt tks) ++ recTks tt tks
  Yes -> [tk] ++ recTks tt tks

--recTksPartially :: TTrie -> [Token] -> [Token]
--recTksPartially tt tks = 

---
-- main
main :: IO ()
main = do (dicfp:fps) <- getArgs
          dic <- readFile dicfp
          let names = map words . lines $ dic
              tt    = foldr (\x t -> insertLex t x) emptyTTrie names
          return ()
