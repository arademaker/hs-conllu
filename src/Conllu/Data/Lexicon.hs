module Conllu.Data.Lexicon where

---
-- imports
import           Conllu.Type
import           Conllu.IO

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
recTks :: TTrie -> [Token] -> [[Token]]
recTks tt tks =
     L.foldr
       (\tks' xs ->
          let res =
                fmap length .
                recLex tt . L.inits . map (fromJust . _form) $
                tks'
          in if isJust res
               then take (fromJust res) tks' : xs
               else xs)
       []
       (L.tails tks)

recLex :: TTrie -> [[String]] -> Maybe [String]
recLex tt =
  fmap snd .
  L.find (\(inT, _) -> inT == Yes) .
  reverse .
  takeWhile (\(inT, _) -> inT == Partially || Yes == inT) .
  map (\ss-> (fst $ memberLex tt ss, ss)) . tail

---
-- correcting
correctTkHead :: Index -> Token -> Token
correctTkHead hi t@SToken{} = t {_dephead = Just hi}
correctToken _hi t = t

correctTkDep :: [Dep] -> Token -> Token
correctTkDep ds t@SToken {_deprel = Just (dep, _)} =
  if dep `notElem` ds
    then t {_deprel = Just (FLAT, "name")}
    else t
correctTkDep _ds t = t

correctLex
  :: [(Index, Index)]
  -> (Index -> Token -> Token) -- correct by ID (inside name)
  -> (Index -> Token -> Token) -- correct by HEAD (pointing to name)
  -> [Token]
  -> [Token]
correctLex _as _fi _fh [] = []
correctLex as fi fh (t:tt) =
  let mi = L.lookup (_ix t) as
      mh = L.lookup (fromMaybe 0 . _dephead $ t) as
      correctRest = correctLex as fi fh tt
  in case (mi, mh) of
       (Nothing, Nothing) -> t : correctRest
       (Nothing, Just h) -> fh h t : correctRest
       (Just i, _) -> fi i t : correctRest

mkCorrectAssoc :: [[Token]] -> [(Index, Index)]
mkCorrectAssoc =
  concatMap (\ts -> map (\t -> (_ix t, _ix $ head ts)) $ tail ts)

correctTks :: TTrie -> [Token] -> [Token]
correctTks tt tks =
  let l = recTks tt (filter isSToken tks)
      as = mkCorrectAssoc l
  in correctLex
       as
       (\i -> correctTkDep [] . correctTkHead i)
       correctTkHead
       tks
---
-- main
main :: IO ()
main = do
  (dicfp:fps) <- getArgs
  dic <- readFile dicfp
  let names = map words . lines $ dic
      tt = beginTTrie names
  ds <- mapM readConlluFile fps
  let ss = concatMap (concatMap (recTks tt . sentSTks) . _sents) ds
  print ss
  return ()
