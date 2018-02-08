module Conllu.Data.Lexicon where

---
-- imports
import Conllu.Type

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid

-- TODO: generalize types
-- TODO: use foldable instance

--- type
data Trie a = Trie Bool (M.Map a (Trie a))
  deriving (Show)

instance Foldable Trie where
  foldr f z (Trie _ m) | M.null m = z
                       | otherwise = foldr (\k v -> foldr f k v) z $ M.toList m


type TTrie = Trie String

emptyMap :: M.Map k v
emptyMap = M.empty

{--
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

membersLex :: TTrie -> [String]
membersLex = mapToList ""
  where
    mapToList k (Trie _ m) = concatMap (\t -> map (k ++) $ membersLex t) $ M.elems m
-- (k -> a -> m)
--}
