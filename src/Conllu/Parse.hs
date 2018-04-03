-- |
-- Module      :  Conllu.Parse
-- Copyright   :  © 2018 bruno cuconato
-- License     :  LPGL-3
--
-- Maintainer  :  bruno cuconato <bcclaro+hackage@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parsers for CoNLL-U format.  the CoNLL-U format is based in the
-- deprecated CoNLL format (defined
-- [here](https://web.archive.org/web/20161105025307/http://ilk.uvt.nl/conll/))
-- and is defined [here](http://universaldependencies.org/format.html)

module Conllu.Parse
  ( Parser
    -- * default parsers
  , document
  , sentence
  , comment
  , token
  -- * customizable parsers
  , ParserC(ParserC)
  , parserC
  -- * CoNLL-U field parsers
  , emptyField
  , tkIndex
  , form
  , lemma
  , upostag
  , xpostag
  , feats
  , dephead
  , deprel
  , deps
  , misc
    -- * utility parsers
  , commentPair
  , listPair
  , stringNot
  , stringWOSpaces
  , stringWSpaces
    -- * parser combinators
  , keyValue
  , maybeEmpty
  , orEmpty
  , listP )
where

---
-- imports

import           Conllu.Type
import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Void
import           System.Environment
import           System.IO

import qualified Text.Megaparsec as M
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = M.Parsec Void String

---
-- conllu parsers
document :: Parser [Sentence]
-- | the default document parser.
document = documentC sentence

documentC :: Parser Sentence -> Parser [Sentence]
-- | the customizable document parser.
documentC s = ws *> s `M.endBy1` blankLine <* M.eof

blankLine :: Parser ()
-- | parse a blank line.
blankLine = lexeme . void $ newline -- spaces shouldn't exist, but no
                                    -- problem being lax here

sentence :: Parser Sentence
-- | the default sentence parser.
sentence = sentenceC comment token

sentenceC :: Parser Comment -> Parser CWord -> Parser Sentence
-- | the customizable sentence parser.
sentenceC c t = liftM2 Sentence (M.many c) (M.some t)

comment :: Parser Comment
-- | parse a comment.
comment = do
  symbol "#" M.<?> "comment starter"
  commentPair <* newline M.<?> "comment content"

word :: Parser CWord
-- | the default token parser.
word =
  tokenC idW form lemma upostag xpostag feats dephead deprel deps misc

tokenC ::
     Parser ID
  -> Parser FORM
  -> Parser LEMMA
  -> Parser UPOS
  -> Parser XPOS
  -> Parser FEATS
  -> Parser ID
  -> Parser (Dep, Maybe String)
  -> Parser DEPS
  -> Parser MISC
  -> Parser CWord
-- | the customizable token parser.
tokenC ixp fop lp upp xpp fep dhp drp dsp mp = do
  i   <- ixp <* tab
  fo  <- fop <* tab
  l   <- lp  <* tab
  up  <- upp <* tab
  xp  <- xpp <* tab
  fe  <- fep <* tab
  dh  <- dhp <* tab
  dr  <- drp <* tab
  dst <- dsp <* tab
  m   <- mp  <* newline
  let ds = (dh, dr) : dst
  return $ mkWord i fo l up xp fe ds m

emptyField :: Parser (Maybe a)
-- | parse an empty field.
emptyField = symbol "_" *> return Nothing M.<?> "empty field"

idW :: Parser ID
-- | parse the ID field, which might be an integer, a range, or a
-- decimal.
idW = do
  ix <- index
  mix <- M.optional metaIndex M.<?> "meta token ID"
  return $
    case mix of
      Nothing         -> SId ix
      Just ('-', eix) -> MId ix eix
      Just ('.', eix) -> EId ix eix
  where
    index :: Parser Index
    index = do
      ix <- M.some digitChar M.<?> "ID"
      return (read ix :: Int)
    indexSep :: Parser IxSep
    indexSep =
      fmap head (symbol "-" M.<|> symbol "." M.<?> "meta separator")
    metaIndex :: Parser (IxSep, Index)
    metaIndex = do
      sep <- indexSep
      ix <- index
      return (sep, ix)

form :: Parser FORM
-- | parse the FORM field.
form = orEmpty stringWSpaces M.<?> "FORM"

lemma :: Parser LEMMA
-- | parse the LEMMA field.
lemma = orEmpty stringWSpaces M.<?> "LEMMA"

upos :: Parser UPOS
-- | parse the UPOS field.
upos = maybeEmpty upos' M.<?> "UPOS"
  where
    upos' :: Parser Pos
    upos' = fmap mkPos stringWOSpaces

xpos :: Parser XPOS
-- | parse the XPOS field.
xpos = maybeEmpty stringWOSpaces M.<?> "XPOS"

feats :: Parser FEATS
-- | parse the FEATS field.
feats = listP (listPair "=" (stringNot "=") (stringNot "\t|")
               M.<?> "feature pair")
        M.<?> "FEATS"

dephead :: Parser ID
-- | parse the HEAD field.
dephead = maybeEmpty idW M.<?> "HEAD"

deprel :: Parser (Dep, Maybe String)
-- | parse the DEPREL field.
deprel = maybeEmpty deprel'

deprel' :: Parser (Dep, Maybe String)
-- | parse a non-empty DEPREL field.
deprel' = do
  dep' <- lexeme dep M.<?> "DEPREL"
  st <- M.option [] $ symbol ":" *> (letters M.<?> "DEPREL subtype")
  return (dep', st)
  where
    letters = lexeme $ M.some (letterChar <|> char '.')
    dep :: Parser Dep
    dep = fmap mkDep letters

deps :: Parser DEPS
-- | parse the DEPS field.
deps = listP (listPair ":" idW deprel' M.<?> "DEPS")

misc :: Parser MISC
-- | parse the MISC field.
misc = orEmpty stringWSpaces M.<?> "MISC"

---
-- utility parsers
commentPair :: Parser Comment
-- | parse a comment pair.
commentPair =
  keyValue "=" (stringNot "=\n\t") (M.option "" stringWSpaces)

listPair :: String -> Parser a -> Parser b -> Parser [(a, b)]
-- | parse a list of pairs.
listPair sep p q = keyValue sep p q `M.sepBy1` symbol "|"

stringNot :: String -> Parser String
-- | parse any chars except the ones provided.
stringNot s = lexeme . M.some $ satisfy (`notElem` s)

stringWOSpaces :: Parser String
-- | parse a string until a space, a tab, or a newline.
stringWOSpaces = stringNot " \t\n"

stringWSpaces :: Parser String
-- | parse a string until a tab or a newline.
stringWSpaces = stringNot "\t\n"

---
-- parser combinators
keyValue :: String -> Parser a -> Parser b -> Parser (a, b)
-- | parse a (key, value) pair.
keyValue sep p q = do
  key <- p
  M.optional $ symbol sep
  value <- q
  return (key, value)


-- | two combinators are needed for parsing the empty field (without
-- lookahead). this has to do with the fact that if we do
--
-- > form <|> emptyField
--
-- we would parse "_" as a non-empty FORM field. but if we did
--
-- > emptyField <|> form
--
-- we would parse "_" in "_something" and then the parser would choke
-- expecting a tab.

maybeEmpty :: Parser a -> Parser (Maybe a)
-- | a parser combinator for parsers that won't parse "_" (e.g., as
-- 'lemma' would).
maybeEmpty p = emptyField M.<|> fmap Just p

orEmpty :: Parser String -> Parser (Maybe String)
-- | a parser combinator for parsers that may parse "_".
orEmpty p = do
  r <- p
  case r of
    "_" -> return Nothing
    _   -> return $ Just r

listP :: Parser [a] -> Parser [a]
-- | parse a list of values that may be an empty field. using a parser
-- that returns a possibly empty list like 'sepBy' and many will
-- return the correct result for the empty field ('_'), but will
-- report it the same as any other syntax error.
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
  , _indexP   :: Parser TkIndex
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
  , _indexP   = tkIndex
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

parserC :: ParserC -> Parser [Sentence]
-- | defines a custom parser of sentences. if you only need to
-- customize one parser (e.g., to parse special comments or a special
-- MISC field), you can do:
-- > parserC ParserC{_commentP = myCommentsParser }
parserC p =
  let i  = _indexP p
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
      t  = tokenC i fo l up xp fs dh dr ds m
      s  = sentenceC c t
  in documentC s
