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

import Conllu.Type

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.IO
import Data.Void

import qualified Text.Megaparsec as M
import Text.Megaparsec.Char

type Parser = M.Parsec Void String

---
-- conllu parsers
document :: Parser [Sentence]
document = documentC sentence

documentC :: Parser Sentence -> Parser [Sentence]
documentC s = M.endBy1 s blankLine <* M.eof

blankLine :: Parser ()
blankLine = litSpaces <* newline -- spaces shouldn't exist, but no
                                 -- problem being lax here (I think)

sentence :: Parser Sentence
sentence = sentenceC comment token

sentenceC :: Parser Comment -> Parser Token -> Parser Sentence
sentenceC c t = liftM2 Sentence (M.many c) (M.some t)

comment :: Parser Comment
comment = do char '#'
             commentPair <* newline

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
emptyField = do char '_'
                return Nothing

index :: Parser Index
index = do ix <- M.some digitChar
           return (read ix :: Index)

indexSep :: Parser IxSep
indexSep = char '-' M.<|> char '.'

form :: Parser Form
form = maybeEmpty stringWSpaces

lemma :: Parser Lemma
lemma = maybeEmpty stringWSpaces

upostag :: Parser PosTag
upostag = maybeEmpty upostag'
  where
    upostag' :: Parser Pos
    upostag' = liftM mkPos stringWOSpaces

xpostag :: Parser Xpostag
xpostag = maybeEmpty stringWOSpaces

feats :: Parser Feats
feats = listP $ listPair '=' (stringNot "=") (stringNot "\t|")

dephead :: Parser Dephead
dephead = maybeEmpty $ symbol index

deprel :: Parser DepRel
deprel = maybeEmpty deprel'

deprel' :: Parser (Dep, Subtype)
deprel' = do dep <- dep
             st  <- M.option [] $ char ':' *> M.some letterChar
             return (dep,st)
  where
    dep :: Parser Dep
    dep = liftM mkDep $ M.some letterChar

deps :: Parser Deps
deps = listP $ listPair ':' index deprel'

misc :: Parser Misc
misc = maybeEmpty stringWSpaces

---
-- utility parsers
litSpaces :: Parser ()
-- because spaces consumes \t and \n
litSpaces = M.skipMany $ char ' '

commentPair :: Parser Comment
commentPair =
  keyValue '=' (stringNot "=\n\t") (M.option [] stringWSpaces)

listPair :: Char -> Parser a -> Parser b -> Parser [(a, b)]
listPair sep p q = M.sepBy1 (keyValue sep p q) (char '|')

stringNot :: String -> Parser String
-- [ ] second litSpaces in symbol is redundant
stringNot s = symbol . M.some $ satisfy (\c -> not $ c `elem` s)

stringWSpaces :: Parser String
stringWSpaces = stringNot "\t\n"

stringWOSpaces :: Parser String
stringWOSpaces = stringNot " \t\n"

---
-- parser combinators
keyValue :: Char -> Parser a -> Parser b -> Parser (a, b)
keyValue sep p q = do key   <- p
                      M.optional $ char sep
                      value <- q
                      return (key, value)

symbol :: Parser a -> Parser a
symbol p = litSpaces *> p <* litSpaces

maybeEmpty :: Parser a -> Parser (Maybe a)
maybeEmpty p = emptyField M.<|> liftM Just p

listP :: Parser [a] -> Parser [a]
-- using a parser that returns a possibly empty list like sepBy and
-- many will return the correct result for the empty filed ('_'), but
-- will report it the same as any other syntax error
listP p = liftM (fromMaybe []) $ maybeEmpty p

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
