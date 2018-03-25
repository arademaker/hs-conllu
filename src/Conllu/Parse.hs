{-# LANGUAGE ApplicativeDo #-}
module Conllu.Parse where

{--

 CoNLL-U parsed file reader.  CoNLL format is defined here:
 https://web.archive.org/web/20161105025307/http://ilk.uvt.nl/conll/
 CoNLL-U/UD format is defined here:
 http://universaldependencies.org/format.html
 http://universaldependencies.org/v2/conll-u.html

--}

---
-- imports

import           Conllu.Type
import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           System.Environment
import           System.IO
import           Data.Void

import qualified Text.Megaparsec as M
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = M.Parsec Void String

---
-- conllu parsers
document :: Parser [Sentence]
document = documentC sentence

documentC :: Parser Sentence -> Parser [Sentence]
documentC s = ws *> s `M.endBy1` blankLine <* M.eof

blankLine :: Parser ()
blankLine = lexeme . void $ newline -- spaces shouldn't exist, but no
                                    -- problem being lax here

sentence :: Parser Sentence
sentence = sentenceC comment token

sentenceC :: Parser Comment -> Parser Token -> Parser Sentence
sentenceC c t = liftM2 Sentence (M.many c) (M.some t)

comment :: Parser Comment
comment = do
  symbol "#" M.<?> "comment starter"
  commentPair <* newline M.<?> "comment content"

token :: Parser Token
token = tokenC index indexSep form lemma upostag xpostag feats
  dephead deprel deps misc

tokenC :: Parser Index -> Parser IxSep -> Parser Form -> Parser Lemma
  -> Parser PosTag -> Parser Xpostag -> Parser Feats
  -> Parser Dephead -> Parser DepRel -> Parser Deps -> Parser Misc
  -> Parser Token
tokenC ix is fo l up xp fe dh dr ds m =
  mkToken <$> ix
  <*> M.optional is
  <*> M.optional ix <* tab
  <*> fo <* tab
  <*> l  <* tab
  <*> up <* tab
  <*> xp <* tab
  <*> fe <* tab
  <*> dh <* tab
  <*> dr <* tab
  <*> ds <* tab
  <*> m <* newline

emptyField :: Parser (Maybe a)
emptyField = symbol "_" *> return Nothing M.<?> "empty field"

index :: Parser Index
index = do
  ix <- (M.try (DecimalIndex <$> do a <- M.some digitChar; d <- char '.'; b <- M.some digitChar; return (read (a ++ [d] ++ b)))) <|>
        (M.try (uncurry RangeIndex <$> ((,) <$> (read <$> M.some digitChar) <*> (char '>' *> (read <$> M.some digitChar))))) <|>
        (IntIndex <$> (read <$> M.some digitChar))
  return ix

indexSep :: Parser IxSep
indexSep = fmap head (symbol "-" M.<|> symbol "." M.<?> "meta separator")

form :: Parser Form
form = orEmpty stringWSpaces M.<?> "FORM"

lemma :: Parser Lemma
lemma = orEmpty stringWSpaces M.<?> "LEMMA"

upostag :: Parser PosTag
upostag = maybeEmpty upostag' M.<?> "UPOSTAG"
  where
    upostag' :: Parser Pos
    upostag' = fmap mkPos stringWOSpaces

xpostag :: Parser Xpostag
xpostag = maybeEmpty stringWOSpaces M.<?> "XPOSTAG"

feats :: Parser Feats
feats = listP (listPair "=" (stringNot "=") (stringNot "\t|") M.<?> "feature pair") M.<?> "FEATS"

dephead :: Parser Dephead
dephead = maybeEmpty index M.<?> "HEAD"

deprel :: Parser DepRel
deprel = maybeEmpty deprel'

deprel' :: Parser (Dep, Subtype)
deprel' = do
  dep' <- lexeme dep M.<?> "DEPREL"
  st <- M.option [] $ symbol ":" *> (letters M.<?> "DEPREL subtype")
  return (dep', st)
  where
    letters = lexeme $ M.some (letterChar <|> char '.')
    dep :: Parser Dep
    dep = fmap mkDep letters

deps :: Parser Deps
deps = listP (listPair ":" index deprel' M.<?> "DEPS pair")

misc :: Parser Misc
misc = orEmpty stringWSpaces M.<?> "MISC"

---
-- utility parsers
commentPair :: Parser Comment
commentPair =
  keyValue "=" (stringNot "=\n\t") (M.option "" stringWSpaces)

listPair :: String -> Parser a -> Parser b -> Parser [(a, b)]
listPair sep p q = keyValue sep p q `M.sepBy1` symbol "|"

stringNot :: String -> Parser String
stringNot s = lexeme . M.some $ satisfy (`notElem` s)

stringWSpaces :: Parser String
stringWSpaces = stringNot "\t\n"

stringWOSpaces :: Parser String
stringWOSpaces = stringNot " \t\n"

---
-- parser combinators
keyValue :: String -> Parser a -> Parser b -> Parser (a, b)
keyValue sep p q = do
  key <- p
  M.optional $ symbol sep
  value <- q
  return (key, value)

maybeEmpty :: Parser a -> Parser (Maybe a)
-- for parsers that won't parse "_"
maybeEmpty p = emptyField M.<|> fmap Just p

orEmpty :: Parser String -> Parser (Maybe String)
-- for parsers that may parse "_"
orEmpty p = do
  r <- p
  case r of
    "_" -> return Nothing
    _   -> return $ Just r

listP :: Parser [a] -> Parser [a]
-- using a parser that returns a possibly empty list like sepBy and
-- many will return the correct result for the empty filed ('_'), but
-- will report it the same as any other syntax error
listP p = fromMaybe [] <$> maybeEmpty p

---
-- lexing
symbol :: String -> Parser String
symbol = L.symbol ws

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

ws :: Parser ()
ws = void $ M.takeWhileP (Just "space") (== ' ')

---
-- customizable parser
data ParserC = ParserC
  { _commentP :: Parser Comment
  , _indexP   :: Parser Index
  , _ixsepP   :: Parser IxSep
  , _formP    :: Parser Form
  , _lemmaP   :: Parser Lemma
  , _upostagP :: Parser PosTag
  , _xpostagP :: Parser Xpostag
  , _featsP   :: Parser Feats
  , _depheadP :: Parser Dephead
  , _deprelP  :: Parser DepRel
  , _depsP    :: Parser Deps
  , _miscP    :: Parser Misc
  } deriving ()

customC :: ParserC
customC = ParserC
  { _commentP = comment
  , _indexP   = index
  , _ixsepP   = indexSep
  , _formP    = form
  , _lemmaP   = lemma
  , _upostagP = upostag
  , _xpostagP = xpostag
  , _featsP   = feats
  , _depheadP = dephead
  , _deprelP  = deprel
  , _depsP    = deps
  , _miscP    = misc
  }

parseC :: ParserC -> Parser [Sentence]
parseC p =
  let i  = _indexP p
      is = _ixsepP p
      fo = _formP p
      l  = _lemmaP p
      up = _upostagP p
      xp = _xpostagP p
      fs = _featsP p
      dh = _depheadP p
      dr = _deprelP p
      ds = _depsP p
      m  = _miscP p
      c  = _commentP p
      t  = tokenC i is fo l up xp fs dh dr ds m
      s  = sentenceC c t
  in documentC s
