{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Char (isAlpha, isAlphaNum)    
import qualified Text.Megaparsec as TM
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text (Text)


-- | Parser Type Synonym
type Parser = TM.Parsec Void Text
-- ^ A concrete Parser where the inner monad is Identity
--   the error component is Void and the input stream is Text

-- | Parser raw output
type RawData t e = [Either (TM.ParseError t e) Sent]
-- ^ see <https://markkarpov.com/tutorial/megaparsec.html#parse-error-definitions>

-- | DEPREL field type synonym
type DEPREL = Maybe (D.EP, Maybe Text)


---
-- conllu parsers
rawSents :: Parser (RawData Text Void)
-- | parse CoNLL-U sentences with recovery.
rawSents = rawSentsC sentence

rawSentsC :: Parser Sent -> Parser (RawData Text Void)
{- | Parse CoNLL-U sentences withRecovery, using a custom parser.
       A sentence block consists of a set of metadata lines or comments ( 1st character is "#").
          See Conllu.Type Comment for more detail
          Two comments are required:
             A sentence ID regex: "# sent_id\s*\=*\s*(.)*" ID must be unique and not contain a space.
             The backslash is reserved
             The sentence  regex: "(^# text(_ww) = (.)*$)" , where ww is a two or three code language specifier.
       WordLines are the block of annotation lines following the metadata block
       WordLines, begin with an Index integer starting at 1 for each new sentence,
            a range of integers, e.g. or decimal number. See Conllu.Type data ID for more detail.
       CoNLL-U annotations consist of 10 fields separated by a tab (\t).
       CoNLL-UP (Plus) may contain more than 10 fields
         See <https://universaldependencies.org/ext-format.html>
       A sentence block ends with one line consisting of a unix lineFeed "\n" and no spaces.
       The Sent Parser utilizes the megaparsec "withRecovery primitive" to "build a parsing error handler"
         for files that do not follow the CoNLL-U format."  And to support verification.
-}
rawSentsC sent =
  TM.between ws TM.eof (TM.endBy1 e lineFeed)
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
-- | parse a blank line.
lineFeed = lexeme . void $ newline -- Spaces shouldn't exist, but no problem being lax here
                                   -- TODO spaces should generate a warning

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
  return $ CW i mf ml mup mxp mfs (rel mdh mdr) ds mm
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

-- | 'ID' separator in meta words
type IxSep   = Char   -- TODO This belongs in the Conllu.Parse Module. Only place it is used.

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
form = orEmpty stringWSpaces TM.<?> "FORM"  -- TODO allow blank

lemma :: Parser LEMMA
-- | parse the LEMMA field.
lemma = orEmpty stringWSpaces TM.<?> "LEMMA"

upos :: Parser UPOS
-- | parse the UPOS field.
upos = maybeEmpty upos'
  where
    upos' :: Parser U.POS
    upos' = TM.choice
       [ U.ADJ   <$ string  "ADJ"     --  adjective
       , U.ADP   <$ string  "ADP"     --  adposition
       , U.ADV   <$ string  "ADV"     --  adverb
       , U.AUX   <$ string  "AUX"     --  auxiliary
       , U.CCONJ <$ string  "CCONJ"   --  coordinating conjunction
       , U.DET   <$ string  "DET"     --  determiner
       , U.INTJ  <$ string  "INTJ"    --  interjection
       , U.NOUN  <$ string  "NOUN"    --  noun
       , U.NUM   <$ string  "NUM"    --  numeral
       , U.PART  <$ string  "PART"    --  particle
       , U.PRON  <$ string  "PRON"    --  pronoun
       , U.PROPN <$ string  "PROPN"   --  proper noun
       , U.PUNCT <$ string  "PUNCT"   --  punctuation
       , U.SCONJ <$ string  "SCONJ"   --  subordinating conjunction
       , U.SYM   <$ string  "SYM"     --  symbol
       , U.VERB  <$ string  "VERB"    --  verb
       , U.X     <$ string  "X"       --  other 
       ]
{-# INLINE upos #-}

xpos :: Parser XPOS
-- | parse the XPOS field.
xpos = maybeEmpty stringWOSpaces TM.<?> "XPOS"

feats :: Parser FEATS
-- | parse the FEATS field.
feats = listP (feat `TM.sepBy` symbol "|" TM.<?> "FEATS")
  where
    feat = do
      k <- lexeme (TM.takeWhileP (Just "feature key") isAlphaNum) -- feature names: [A-Z][A-Za-z0-9]*(\[[a-z0-9]+\])?
      ft <-
        TM.optional $
        TM.between (symbol "[") (symbol "]") (TM.takeWhileP (Just "Feature") isAlphaNum) --feature-layer between []
      _  <- symbol "="
      vs <- fvalue `TM.sepBy1` symbol ","  
      return $ Feat k vs ft
    fvalue = lexeme (TM.takeWhileP (Just "feature value") isAlphaNum)   -- featurevalues [A-Z0-9][A-Za-z0-9]*

deprel :: Parser DEPREL
-- | parse the DEPREL field.
deprel = maybeEmpty deprel'

dep :: Parser D.EP
dep = TM.choice
  [ D.REF        <$ string  "ref"
  , D.ACL        <$ string  "acl"
  , D.ADVCL      <$ string  "advcl"
  , D.ADVMOD     <$ string  "advmod"
  , D.AMOD       <$ string  "amod"
  , D.APPOS      <$ string  "appos"
  , D.AUX        <$ string  "aux"
  , D.CASE       <$ string  "case"
  , D.CCOMP      <$ string  "ccomp"
  , D.CC         <$ string  "cc"
  , D.CLF        <$ string  "clf"
  , D.COMPOUND   <$ string  "compound"
  , D.CONJ       <$ string  "conj"
  , D.COP        <$ string  "cop"
  , D.CSUBJ      <$ string  "csubj"
  , D.DEP        <$ string  "dep"
  , D.DET        <$ string  "det"
  , D.DISCOURSE  <$ string  "discourse"
  , D.DISLOCATED <$ string  "dislocated"
  , D.EXPL       <$ string  "expl"
  , D.FIXED      <$ string  "fixed"
  , D.FLAT       <$ string  "flat"
  , D.GOESWITH   <$ string  "goeswith"
  , D.IOBJ       <$ string  "iobj"
  , D.LIST       <$ string  "list"
  , D.MARK       <$ string  "mark"
  , D.NMOD       <$ string  "nmod"
  , D.NSUBJ      <$ string  "nsubj"
  , D.NUMMOD     <$ string  "nummod"
  , D.OBJ        <$ string  "obj"
  , D.OBJ        <$ string  "obj"
  , D.OBL        <$ string  "obl"
  , D.ORPHAN     <$ string  "orphan"
  , D.PARATAXIS  <$ string  "parataxis"
  , D.PUNCT      <$ string  "punct"
  , D.REPARANDUM <$ string  "reparandum"
  , D.ROOT       <$ string  "root"
  , D.VOCATIVE   <$ string  "vocative"
  , D.XCOMP      <$ string  "xcomp"
  ]
{-# INLINE dep #-}

deprel' :: Parser (D.EP, Maybe Text)
-- | parse a non-empty DEPREL field.
deprel' = liftM2 (,) dep subdeprel
  where
    subdeprel :: Parser (Maybe Text)
    subdeprel = TM.optional (symbol ":" *> TM.takeWhile1P (Just "DEPREL subtype") isAlpha )

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
-- | parse a comment pair.stack build

commentPair =
  keyValue "=" (stringNot "=\n\t") (TM.option "" stringWSpaces)

listPair :: Text -> Parser a -> Parser b -> Parser [(a, b)]
-- | parse a list of pairs.
listPair sep p q = keyValue sep p q `TM.sepBy1` symbol "|"

stringNot :: String -> Parser Text
-- | parse any chars except the ones provided.
stringNot s = lexeme $ TM.takeWhile1P Nothing (`notElem` s)

stringWOSpaces :: Parser Text
-- | parse a string until a space, a tab, or a newline.
stringWOSpaces = stringNot " \t\n"

stringWSpaces :: Parser Text
-- | parse a string until a tab or a newline.
stringWSpaces = stringNot "\t\n"

{-
letters :: Parser [Char]
-- | parse a string of letters.
letters = lexeme $ TM.some letterChar
-}

---
-- parser combinators
keyValue :: Text -> Parser a -> Parser b -> Parser (a, b)
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

orEmpty :: Parser Text -> Parser (Maybe Text)
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
symbol :: Text -> Parser Text
symbol = L.symbol ws

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

ws :: Parser ()
ws = void $ TM.takeWhileP (Just "space") (== ' ')

{- | Conllu Format
      Annotations are encoded in plain text files (UTF-8, normalized to NFC,
      using only the LF character as line break, including an LF character at the end of file)
      with three types of lines:
      1) Comment lines starting with hash (#)Comment lines starting with hash (#)
      2) Blank lines marking sentence boundaries.
      3) Word lines containing the annotation of a word/token in 10 fields
         separated by single tab characters; as declared below.
      See <https://universaldependencies.org/ext-format.html>

    Conllu-Plus formats (.conllup) can be supported by creating
    data ConlluPlus = {_conllu :: ParserC , _plus :: PlusParser}
    see <https://universaldependencies.org/ext-format.html>
-}
-- customizable parser
data ParserC = ParserC          -- | this is the ConlluParser ?
  { _commentP :: Parser Comment
  , _idP      :: Parser ID
  , _formP    :: Parser FORM
  , _lemmaP   :: Parser LEMMA
  , _upostagP :: Parser UPOS
  , _xpostagP :: Parser XPOS
  , _featsP   :: Parser FEATS
  , _head     :: Text         -- | added this slot, need to create a head column parser
  , _deprelP  :: Parser DEPREL
  , _depsP    :: Parser DEPS
  , _miscP    :: Parser MISC
  } deriving ()

{-
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
-}

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
  :: Parser Sent        -- ^ the sentence parser to be used.
  -> FilePath           -- ^ the source whose stream is being supplied in the  next argument (may be "" for no file)
  -> Text               -- ^ input for parser
  -> Either String Doc  -- ^ Either (ParseErrorBundle s e) a
-- | parse a CoNLL-U formatted file using a customized parser.
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


parseConllu :: FilePath -> Text -> Either String Doc
-- | parse a CoNLL-U document using the default parser.
parseConllu = parseConlluWith sentence
