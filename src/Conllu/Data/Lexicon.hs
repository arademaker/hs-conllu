module Conllu.Data.Lexicon where

---
-- imports
import           Conllu.Type

import qualified Data.List as L
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

beginTTrie :: [[String]] -> TTrie
beginTTrie = L.foldr (flip insertLex) emptyTTrie

memberLex :: TTrie -> [String] -> (InTrie, Maybe TTrie)
memberLex tt@(Trie v _) [] =
  if v
    then (Yes, Nothing)
    else (Partially, Just tt)
memberLex (Trie _ m) (x:xt) =
  let mt = M.lookup x m
  in case mt of
       Nothing -> (No, Nothing)
       Just t -> memberLex t xt

---
-- recognizing
--recTks :: TTrie -> [Token] -> [[Token]]
recTks tt =
  map
    (\tks ->
       let res =
             fmap length .
             recLex tt . L.inits . map (fromJust . _form) $ tks
       in if isJust res
            then take (fromJust res) tks
            else []) .
  L.tails 
--  catMaybes . map recLex tt . L.tails . map (\t -> (t, fromJust . _form))

-- > recLex tt ["joão", "das", "couves", "das", "trevas", undefined]
-- > == Just ["jo\227o","das","couves"]
recLex :: TTrie -> [[String]] -> Maybe [String]
recLex tt =
  fmap snd .
  L.find (\(inT, _) -> inT == Yes) .
  reverse .
  takeWhile (\(inT, _) -> inT == Partially || Yes == inT) .
  map (\ss-> (fst $ memberLex tt ss, ss)) . tail

---
-- main
main :: IO ()
main = do (dicfp:fps) <- getArgs
          dic <- readFile dicfp
          let names = map words . lines $ dic
              tt    = beginTTrie names
          return ()
