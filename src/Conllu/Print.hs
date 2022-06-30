-- |
-- Module      :  Conllu.Print
-- Copyright   :  © 2018 bruno cuconato
-- License     :  LPGL-3
--
-- Maintainer  :  bruno cuconato <bcclaro+hackage@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- prints CoNLL-U.

{-# LANGUAGE OverloadedStrings #-}
module Conllu.Print
where

import           Conllu.Type
import           Data.List  (group, transpose ) --, concat)
import           Data.Maybe (fromMaybe, fromJust) --, listToMaybe)
import qualified Data.Text      as T

---
-- printing
printDoc :: Doc -> T.Text
-- | prepares a list of sentences for printing:
printDoc doc =  T.intercalate "\n" $ T.intercalate "\t" <$> (concat $ printSent <$> doc) 

printSent :: Sent -> [[T.Text]]
-- | prepares a sentence for printing
-- | transpose regroups the sentence fields into a list of word lines
--   Sentences consist of one or more word lines, and word lines contain the fields in printWords
printSent sentence =  group     (printComment <$> _meta  sentence)
                   <> transpose (printWords    $  _words sentence)

printComment :: Comment -> T.Text
-- | reconstructs a comment line from its parsed representation
--   "# first comment < = second comment>"
-- TODO GVF fix comment parse -- should not include space 
printComment comment = "# "
                     <> fst comment
                     <> if T.null $ snd comment
                        then ""
                        else " = "
                     <> snd comment

printWords :: [CW a]  ->  [[T.Text]]
-- | reconstructs the 10 fields from parsed representation
printWords cw =  (<$> cw) <$>
                [ printID        . _id
                , fromMaybe "_"  . _form 
                , fromMaybe "_"  . _lemma 
                , printUPOS      . _upos  
                , printXPOS      . _xpos  
                , printFEATS     . _feats 
                , printHEAD      . _rel   
                , printDEPREL    . _rel   
--                , printDEPS    . _deps  
                , printMISC      . _misc]
---
-- field printers
printID :: ID -> T.Text
printID id' =
    case id' of
      SID id1     -> T.pack $ show id1
      MID id1 id2 -> T.pack $ show id1 <> "-" <>  show id2
      EID id1 id2 -> T.pack $ show id1 <> "." <>  show id2

printUPOS :: UPOS -> T.Text
printUPOS    upos =  T.pack $ maybe "_" show upos

printXPOS :: XPOS -> T.Text
printXPOS    xpos =  T.toLower . T.pack $ maybe "_" show xpos

printFEATS :: FEATS -> T.Text
printFEATS = printList printFeat
  where
    printFeat Feat {_feat = f, _featValues = vs, _featType = mft} =
      let fts = maybe "" (\ft -> "[" <> ft <> "]") mft
      in f <> fts <> "=" <> T.intercalate "," vs

printHEAD :: Maybe Rel -> T.Text
printHEAD    Nothing   =  "_"
printHEAD   (Just rel) = printID (_head rel)

printDEPREL :: Maybe Rel -> T.Text
printDEPREL   rel = deprel <> subdep
  where
    deprel = T.toLower . T.pack $ maybe "_" (show . _deprel) rel
    subdep =  maybe "" (maybe "" (":" <>) . _subdep) rel


{-
printDEPS :: DEPS -> T.Text -- DEPS = [REL]
printDEPS deps = (\x y z -> T.intercalate ":" [x,y,z]) 
              <$> printHEAD 
              <*> printDEPREL 
              <*> (T.intercalate ":" <$> printREST) 
              <$> listToMaybe . deps
-}


{-
 (<$> (listToMaybe <$> deps)) <$> [
  printHEAD, printDEPREL, T.intercalate ":" <$> printREST]
  
(\x y z -> T.intercalate ":" [x,y,z]) <$> printHEAD <*> printDEPREL <*> (T.intercalate ":" <$> printREST) <$> (listToMaybe <$> (_deps <$> cw))
 
-}

printREST :: Maybe Rel -> [T.Text]
printREST = fromMaybe [] <$> _rest . fromJust

printMISC :: MISC -> T.Text
printMISC = T.pack . maybe "" show

---
-- utility printers
printList :: (a -> T.Text) -> [a] -> T.Text
printList f = nullToStr . T.intercalate "|" . map f
  where
    nullToStr :: T.Text -> T.Text
    nullToStr xs =
      if T.null xs
        then "_"
        else xs



