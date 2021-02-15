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
  -- * parsers
  , parseConlluWith
  , parseConllu
  -- * customizable parsers
  , ParserC(ParserC)
  , parserC
  -- * default parsers
  , rawSents
  , sentence
  , comment
  , word
  -- * CoNLL-U field parsers
  , emptyField
  , idW
  , form
  , lemma
  , upos
  , xpos
  , feats
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
import qualified Conllu.DeprelTagset as D
import qualified Conllu.UposTagset as U

import           Control.Monad (void, liftM2)
import           Data.Either
import           Data.Maybe
import           Data.Void (Void)

import qualified Text.Megaparsec as TM 
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser type synonym
type Parser = TM.Parsec Void String

-- | Parser raw output
type RawData t e = [Either (TM.ParseError t e) Sent]

-- | DEPREL field type synonym
type DEPREL = Maybe (D.EP, Maybe String)


---
-- conllu parsers
rawSents :: Parser (RawData String Void)
-- | parse CoNLL-U sentences with recovery.
rawSents = rawSentsC sentence

rawSentsC :: Parser Sent -> Parser (RawData String Void)
-- | parse CoNLL-U sentences with recovery, using a custom parser.
rawSentsC sent = TM.between ws TM.eof (TM.endBy1 e lineFeed)
  where
    e = TM.withRecovery recover (Right <$> sent)
    recover err =
      Left err <$
      TM.skipManyTill TM.anySingle
      -- if parser consumes the first newline but can't parse the
      -- second, it breaks; it can't consume the second one, because
      -- that one has to be consumed by the endBy1
      (TM.try $ lineFeed *> TM.lookAhead lineFeed)

lineFeed :: Parser ()
-- | parse a blank line. Spaces shouldn't exist, but no problem being lax here
lineFeed = lexeme . void $ newline 

sentence :: Parser Sent
-- | the default sentence parser.
sentence = sentenceC comment word

sentenceC :: Parser Comment -> Parser (CW AW) -> Parser Sent
-- | the customizable sentence parser.
sentenceC c t = liftM2 Sent (TM.many c) (TM.some t)

comment :: Parser Comment
-- | parse a comment.
comment =
  (symbol "#" TM.<?> "comment starter") *> commentPair <*
  lineFeed TM.<?> "comment content"

word :: Parser (CW AW)
-- | the default word parser.
word =
  wordC idW form lemma upos xpos feats deprel deps misc

wordC ::
     Parser ID
  -> Parser FORM
  -> Parser LEMMA
  -> Parser UPOS
  -> Parser XPOS
  -> Parser FEATS
  -> Parser DEPREL
  -> Parser DEPS
  -> Parser MISC
  -> Parser (CW AW)
-- | the customizable token parser.
wordC ixp fop lp upp xpp fsp drp dsp mp = do
  i   <- ixp <* tab
  mf  <- fop <* tab
  ml  <- lp  <* tab
  mup <- upp <* tab
  mxp <- xpp <* tab
  mfs <- fsp <* tab
  mdh <- dhp <* tab
  mdr <- drp <* tab
  ds  <- dsp <* tab
  mm  <- mp  <* lineFeed
  return $ mkAW i mf ml mup mxp mfs (rel mdh mdr) ds mm
  where
    dhp = maybeEmpty ixp TM.<?> "HEAD"
    rel :: Maybe ID -> DEPREL -> Maybe Rel
    rel mdh mdr = do
      dh <- mdh
      (dr, sdr) <- mdr
      return $ Rel dh dr sdr Nothing

emptyField :: Parser (Maybe a)
-- | parse an empty field.
emptyField = symbol "_" *> return Nothing TM.<?> "empty field (_)"

idW :: Parser ID
-- | parse the ID field, which might be an integer, a range, or a
-- decimal.
idW = do
  ix <- index
  mix <- TM.optional metaIndex TM.<?> "meta token ID"
  return $
    case mix of
      Nothing             -> SID ix
      Just (Left _, eix)  -> MID ix eix
      Just (Right _, eix) -> EID ix eix
  where
    index :: Parser Index
    index = do
      ix <- TM.some digitChar TM.<?> "ID"
      return (read ix :: Int)
    indexSep :: Parser (Either IxSep IxSep)
    indexSep = TM.eitherP (char '-') (char '.') TM.<?> "meta separator"
    metaIndex :: Parser (Either IxSep IxSep, Index)
    metaIndex = do
      sep <- indexSep
      ix <- index
      return (sep, ix)

form :: Parser FORM
-- | parse the FORM field.
form = orEmpty stringWSpaces TM.<?> "FORM"

lemma :: Parser LEMMA
-- | parse the LEMMA field.
lemma = orEmpty stringWSpaces TM.<?> "LEMMA"

upos :: Parser UPOS
-- | parse the UPOS field.
upos = maybeEmpty upos'
  where
    upos' :: Parser U.POS
    upos' = fmap mkUPOS $ TM.choice $ fmap (string' . show) [U.ADJ .. U.X]

xpos :: Parser XPOS
-- | parse the XPOS field.
xpos = maybeEmpty stringWOSpaces TM.<?> "XPOS"

feats :: Parser FEATS
-- | parse the FEATS field.
feats = listP (feat `TM.sepBy` symbol "|" TM.<?> "FEATS")
  where
    feat = do
      k  <- lexeme (TM.some alphaNumChar TM.<?> "feature key")
      ft <-
        TM.optional $
        TM.between (symbol "[") (symbol "]") (TM.some alphaNumChar)
      _  <- symbol "="
      vs <- fvalue `TM.sepBy1` symbol ","
      return $ Feat k vs ft
    fvalue = lexeme (TM.some alphaNumChar TM.<?> "feature value")

deprel :: Parser DEPREL
-- | parse the DEPREL field.
deprel = maybeEmpty deprel'

dep :: Parser D.EP
dep = fmap mkDEP $ TM.choice $ fmap (string' . show) [D.ACL .. D.XCOMP]

deprel' :: Parser (D.EP, Maybe String)
-- | parse a non-empty DEPREL field.
deprel' = liftM2 (,) dep subdeprel
  where
    subdeprel :: Parser (Maybe String)
    subdeprel = TM.optional (symbol ":" *> letters TM.<?> "DEPREL subtype")

deps :: Parser DEPS
-- | parse the DEPS field.
deps = listP (eDep `TM.sepBy` symbol "|" TM.<?> "DEPS")
  where
    eDep = do
      h <- idW TM.<?> "enhanced dependency HEAD"
      _ <- sep
      d <- dep TM.<?> "enhanced dependency DEPREL"
      restI <-
        TM.optional
          (sep *>
           stringNot "\t| :" `TM.sepBy` sep TM.<?>
           "enhanced dependency information")
      return $ Rel h d Nothing restI
    sep = symbol ":"

misc :: Parser MISC
-- | parse the MISC field.
misc = orEmpty stringWSpaces TM.<?> "MISC"

---
-- utility parsers
commentPair :: Parser Comment
-- | parse a comment pair.
commentPair =
  keyValue "=" (stringNot "=\n\t") (TM.option "" stringWSpaces)

listPair :: String -> Parser a -> Parser b -> Parser [(a, b)]
-- | parse a list of pairs.
listPair sep p q = keyValue sep p q `TM.sepBy1` symbol "|"

stringNot :: String -> Parser String
-- | parse any chars except the ones provided.
stringNot s = lexeme $ TM.takeWhile1P Nothing (`notElem` s)

stringWOSpaces :: Parser String
-- | parse a string until a space, a tab, or a newline.
stringWOSpaces = stringNot " \t\n"

stringWSpaces :: Parser String
-- | parse a string until a tab or a newline.
stringWSpaces = stringNot "\t\n"

letters :: Parser String
-- | parse a string of letters.
letters = lexeme $ TM.some letterChar

---
-- parser combinators
keyValue :: String -> Parser a -> Parser b -> Parser (a, b)
-- | parse a (key, value) pair.
keyValue sep p q = do
  key   <- p
  _     <- TM.optional $ symbol sep
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
maybeEmpty p = emptyField TM.<|> fmap Just p

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
ws = void $ TM.takeWhileP (Just "space") (== ' ')

---
-- customizable parser
data ParserC = ParserC
  { _commentP :: Parser Comment
  , _idP      :: Parser ID
  , _formP    :: Parser FORM
  , _lemmaP   :: Parser LEMMA
  , _upostagP :: Parser UPOS
  , _xpostagP :: Parser XPOS
  , _featsP   :: Parser FEATS
  , _deprelP  :: Parser DEPREL
  , _depsP    :: Parser DEPS
  , _miscP    :: Parser MISC
  } deriving ()

customC :: ParserC
customC = ParserC
  { _commentP = comment
  , _idP      = idW
  , _formP    = form
  , _lemmaP   = lemma
  , _upostagP = upos
  , _xpostagP = xpos
  , _featsP   = feats
  , _deprelP  = deprel
  , _depsP    = deps
  , _miscP    = misc
  }

parserC :: ParserC -> Parser Sent
-- | defines a custom parser of sentences. if you only need to
-- customize one field parser (e.g., to parse special comments or a
-- special MISC field), you can do:
--
-- @
-- parserC ParserC{_commentP = myCommentsParser }
-- @
parserC p =
  let i  = _idP p
      f  = _formP p
      l  = _lemmaP p
      up = _upostagP p
      xp = _xpostagP p
      fs = _featsP p
      dr = _deprelP p
      ds = _depsP p
      m  = _miscP p
      c  = _commentP p
      w  = wordC i f l up xp fs dr ds m
      s  = sentenceC c w
  in s

---
-- parse
parseConlluWith
  :: Parser Sent -- ^ the sentence parser to be used.
  -> FilePath    -- ^ the source whose stream is being supplied in the
                 -- next argument (may be "" for no file)
  -> String      -- ^ stream to be parsed
  -> Either String Doc
-- | parse a CoNLL-U document using a customized parser.
parseConlluWith p fp s =
  case TM.parse doc fp s of
    Left err -> Left $ TM.errorBundlePretty err
    Right d  ->
      let (ls, rs) = partitionEithers d
      in if null ls
           then Right rs
           else Left $ concatMap TM.parseErrorPretty ls
  where
    doc = rawSentsC p


parseConllu :: FilePath -> String -> Either String Doc
-- | parse a CoNLL-U document using the default parser.
parseConllu = parseConlluWith sentence
