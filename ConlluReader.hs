{--

 CoNLL-U parsed file reader.  CoNLL format is defined here:
 https://web.archive.org/web/20161105025307/http://ilk.uvt.nl/conll/
 CoNLL-U/UD format is defined here:
 http://universaldependencies.org/format.html
 http://universaldependencies.org/v2/conll-u.html

-- TODO: Convert String -> Text
-- TODO: test if featsP parses correctly

-- TODO: warn of spaces in fields where they are not allowed (FORM and
-- LEMMA). do this in symbol function.

--}

module ConlluReader where

import Ud

--import Control.Applicative
import Data.Char
import Data.List
--import Data.List.Split
import Data.Maybe
import Control.Monad
import System.IO
import Text.Parsec hiding (token,tokens)
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.String


document :: Parser [Sentence]
document = do ss <- endBy1 sentence blankLine
              eof
              return ss

blankLine :: Parser ()
blankLine = do litSpaces -- shouldn't exist, but no problem being lax
                         -- here
               newline
               return ()

litSpaces :: Parser ()
-- because spaces consumes \t and \n
litSpaces = do skipMany $ char ' '

sentence :: Parser Sentence
sentence = do cs <- many comment
              ts <- many1 token
              return Sentence { meta = cs, tokens = ts }

comment :: Parser Comment
comment = do char '#'
             stringPair

stringPair :: Parser (String, String)
stringPair = do
  parsekeyValue '=' (stringNot "=\n\t ") stringWSpaces

keyValue :: Char -> Parser a -> Parser b -> Parser (a, b)
keyValue sep p q = do key   <- p
                      char sep
                      value <- q
                      return (p, q)

stringNot :: String -> Parser String
-- [ ] second litSpaces is redundant
stringNot s = do symbol . many1 $ noneOf s

symbol :: Parser a -> Parser a
symbol p = do litSpaces
              x <- p
              litSpaces
              return x

token :: Parser Token
-- how to parse tabs elegantly?
-- new combinator? no.
-- check liftM5 and above for no variable parsing, see
-- graham's book
token = do ix    <- sTokenId
           ixSep <- optionMaybe $ choice [char '-', char '.']
           ixEnd <- optionMaybe index
           tab
           fo    <- formP
           tab
           l     <- lemmaP
           tab
           up    <- upostagP
           tab
           xp    <- xpostagP
           tab
           fe    <- featsP
           tab
           h     <- depheadP
           tab
           dr    <- deprelP
           tab
           d     <- depsP
           tab
           m     <- miscP
           return $ mkToken ix ixSep ixEnd fo l up xp f h dr d m

index :: Parser Index
index = do ix <- many1 digit
           return (read ix :: Index)

formP :: Parser Form
formP = do stringWSpaces

lemmaP :: Parser Lemma
lemmaP = do stringWSpaces

stringWSpaces :: Parser String
stringWSpaces = do stringNot "\t\n"

upostagP :: Parser PosTag
upostagP = do liftM mkPosTag $ stringWOSpaces

xpostagP :: Parser Xpostag
xpostagP = do stringWOSpaces

stringWOSpaces :: Parser String
stringWOSpaces = do stringNot " \t\n"

featsP :: Parser Feats
-- featsP' with sepBy will return the correct result for the empty
-- filed ('_'), but will report it the same as any other syntax error
featsP = do mightBeEmpty [] featsP'

mightBeEmpty :: a -> Parser a -> Parser a
mightBeEmpty e p = do choice [notOption e emptyField, p]

notOption :: a -> Parsec String u b -> Parsec String u a
notOption x p = do r <- optionMaybe p
                   case r of
                     Just _ -> return x
                     Nothing -> do parserZero

emptyField :: Parser ()
emptyField = do symbol $ char '_'
                return ()

featsP' :: Parser Feats
featsP' = do sepBy1 stringPair (char '|')

depheadP :: Parser Index
depheadP = do headIx <- symbol $ many1 digit
              return (read headIx :: Index)

deprelP :: Parser DepRel
deprelP = do liftM mkDepRel string

depsP :: Parser Deps
depsP = do mightBeEmpty [] depsP'

depsP' :: Parser Deps
depsP' = do sepBy1 (keyValue ':' index deprelP) (char '|')

miscP :: Parser Misc
miscP = do stringWSpaces

{--
parseLine' :: [String] -> Maybe Token
parseLine' (s1:s2:s3:s4:s5:s6:s7:s8:s9:s10:_)
  | isJust ix && all isDigit s7 && (na s9 || all isDigit s9) =
      Just $ Token
      { ix       = fromJust ix
      , form     = s2
      , lemma    = if s3==unk then [] else splitOn "|" s3
      , cpostag  = s4
      , postag   = s5
      , feats    = if na s6 then [] else splitOn "|" s6
      , dephead  = read s7
      , deprel   = s8
      , pdephead = if na s9  then Nothing else Just $ read s9
      , pdeprel  = if na s10 then Nothing else Just s10 }
  | otherwise   = Nothing
  where ix = readIx s1
        na = (`elem` ["_","-"])
parseLine' _ = Nothing


-- skips sentences that contain unparsable lines
readCorpusStr :: String -> Corpus
readCorpusStr =
  mapMaybe (fmap mkSentence . sequence . map parseLine) .
  split (dropInitBlank . dropFinalBlank . dropDelims . condense $ whenElt emptyLine) .
  lines
  where emptyLine = null . words

readCorpus :: FilePath -> IO Corpus
readCorpus f = readCorpusStr <$> readFile f

showCorpus :: Corpus -> String
showCorpus = unlines . map (unlines . map showToken . sentenceTokens)
  where showToken t = intercalate "\t" $ map (\f -> f t)
          [show . ix, form, showLemma . lemma, cpostag,
           postag, const "_", show . dephead, deprel,
           fromMaybe "_" . fmap show . pdephead, fromMaybe "_" . pdeprel]
        showLemma l | null l    = unk
                    | otherwise = intercalate "|" l

headToken :: Sentence -> Token -> Maybe Token
headToken s t = case dephead t of
              0 -> Nothing
              i -> M.lookup i s

sentenceTokens :: Sentence -> [Token]
sentenceTokens = M.elems

depTokens :: Sentence -> Token -> [Token]
depTokens s t = filter ((==ix t) . dephead) $ M.elems s

depTokensBy :: DepRel -> Sentence -> Token -> [Token]
depTokensBy r s = filter ((==r) . deprel) . depTokens s

adjTokens :: Sentence -> Token -> [Token]
adjTokens s t = catMaybes [headToken s t] ++ depTokens s t

getLP :: Token -> [LemmaPos]
getLP t = [(l,cpostag t) | l <- lemma t]

-- fallbacks to wordform in case of unknown lemma
getLP' :: Token -> [LemmaPos]
getLP' t = if unknownLemma t then [(form t,postag t)] else getLP t

showLP :: LemmaPos -> String
showLP (l,[]) = l
showLP (l,p)  = l ++ posSep ++ p

lemmaPos :: Token -> [String]
lemmaPos = map showLP . getLP

lemma' :: Token -> [String]
lemma' t = case lemma t of
  [] -> [form t]
  ls -> ls

lemmaPos' :: Token -> [String]
lemmaPos' = map showLP . getLP'

formPos :: Token -> String
formPos t = case (form t, cpostag t) of
  (w,[]) -> w
  (w,p)  -> w ++ posSep ++ p

unknownLemma :: Token -> Bool
unknownLemma = null . lemma

--}
