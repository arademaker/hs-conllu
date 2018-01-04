{--

 CoNLL-U parsed file reader.  CoNLL format is defined here:
 https://web.archive.org/web/20161105025307/http://ilk.uvt.nl/conll/
 CoNLL-U/UD format is defined here:
 http://universaldependencies.org/format.html

-- TODO: Convert String -> Text

--}

module ConlluReader where

import Ud

--import Control.Applicative
import Data.Char
import Data.List
--import Data.List.Split
import Data.Maybe
import System.IO
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char


document :: String -> Parser [Sentence]
document = do endBy1 sentence blankLine

blankLine :: String -> Parser ()
blankLine = do litSpaces -- shouldn't exist, but no problem being lax
                         -- here
               newline

litSpaces :: String -> Parser ()
-- because spaces consumes \t and \n
litSpaces = skipMany $ char ' '

sentence :: String -> Parser Sentence
sentence = do cs <- many comment
              ts <- many1 token
              return Sentence { meta = cs, tokens = ts }

comment :: String -> Parser Comment
comment = do char '#'
             litSpaces
             key   <- many1 noneOf "=\n"
             litSpaces
             value <- many1 noneOf "\n"
             litSpaces
             return (key, value)

token :: String -> Parser Token
token = do id      <- choice [mTokenId, sTokenId]
           form    <- form
           lemma   <- lemma
           upostag <- upostag
           xpostag <- xpostag
           feats   <- feats
           dephead <- dephead
           deprel  <- deprel
           deps    <- deps
           misc    <- misc
           -- check liftM5 and above for no variable parsing, see
           -- graham's book
           return mkToken id form lemma upostag xpostag feats dephead deprl deps misc

mTokenId :: String -> Parser (Tstart, Tend)
mTokenId = do start <- many1 digit
              char '-'
              end <- many1 digit
              tab
              return (start, end)

sTokenId :: String -> Parser Tid
sTokenId = do ix <- many1 digit
              litSpaces
              tab
              return ix

form :: Form
form = symbol

lemma :: Lemma
lemma = symbol

xpostag :: Xpostag
xpostag = symbol

misc :: Misc
misc = symbol

symbol :: String -> Parser String
symbol = do litSpaces
            s <- many1 $ noneOf "\t\n"
            litSpaces
            tab
            return s

upostag :: String -> Parser PosTag
upostag = do s <- symbol
             return mkPosTag s

feats' :: String -> Parser Feats

feats :: String -> Parser Feats
feats = do fs <- choice [notOption [] emptyField, feats']
           return fs

emptyField :: String -> Parser ()
emptyField = do litSpaces
                char '_'
                litSpaces

notOption :: a -> Parsec String u b -> Parsec String u a
notOption x p = do r <- optionMaybe p
                   case r of
                     Just _ -> return x
                     Nothing -> do parserZero

{-
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
