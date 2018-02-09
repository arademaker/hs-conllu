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

memberLex :: TTrie -> [String] -> Bool
-- terrible code
memberLex (Trie v _) [] = v
memberLex (Trie _ m) [x] =
  memberLex (M.findWithDefault emptyTTrie x m) []
memberLex (Trie _ m) (x:xt) =
  let tt = M.findWithDefault emptyTTrie x m
  in if isEmptyTTrie tt
       then False
       else memberLex tt xt

---
-- recognizing

---
-- main
main :: IO ()
main = do (dicfp:fps) <- getArgs
          dic <- readFile dicfp
          let names = map words . lines $ dic
              tt    = foldr (\x t -> insertLex t x) emptyTTrie names
          return ()
