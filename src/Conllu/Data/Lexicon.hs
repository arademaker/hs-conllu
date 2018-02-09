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
recTks :: TTrie -> [Token] -> [[Token]]
{-- maybe use tails? and takeWhile Partially/Yes
recTks tt (tk:tks) =
  case memberLex tt [fromJust $ _form tk] of
    (No, _) -> recTksRest
    (Partially, Just tt') ->
      recTksPartially (D.singleton tk) tt' tks ++ recTksRest
    (Yes, _) -> tk : recTksRest
  where
    recTksRest = recTks tt tks
    recTksPartially dl tt' (tk':tks') =
      case memberLex tt' [fromJust $ _form tk'] of
        (No, _) -> []
        (Partially, Just tt'') ->
          recTksPartially (D.snoc dl tk') tt'' tks'
        (Yes, _) -> D.toList $ D.snoc dl tk'
--}
recTks tt = filter (isJust . recLex tt . map (fromJust . _form)) . L.tails

-- > recLex tt ["joão", "das", "couves", "das", "trevas", undefined]
-- > == Just ["jo\227o","das","couves"]
recLex :: TTrie -> [String] -> Maybe [String]
recLex tt =
  fmap snd .
  L.find (\(inT, _) -> inT == Yes) .
  reverse .
  takeWhile (\(inT, _) -> inT == Partially || Yes == inT) .
  map (\s -> (fst $ memberLex tt s, s)) . tail . L.inits

---
-- main
main :: IO ()
main = do (dicfp:fps) <- getArgs
          dic <- readFile dicfp
          let names = map words . lines $ dic
              tt    = foldr (flip insertLex) emptyTTrie names
          return ()
