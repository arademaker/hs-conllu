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

-- lib imports
import Conllu.Type

-- stdlib
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.IO

-- hackage
import Text.Parsec hiding (token)
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char

---
-- conllu parsers
document :: Parser [Sentence]
document = endBy1 sentence blankLine <* eof

blankLine :: Parser ()
blankLine = litSpaces <* newline -- spaces shouldn't exist, but no
                                    -- problem being lax here (I think)

sentence :: Parser Sentence
sentence = liftM2 Sentence (many comment) (many1 token)

comment :: Parser Comment
comment = do char '#'
             commentPair <* newline

token :: Parser Token
-- check liftM5 and above for no variable parsing, see
-- graham's book
token = mkToken <$> index
             <*> optionMaybe indexSep
             <*> optionMaybe index <* tab
             <*> form <* tab
             <*> lemma <* tab
             <*> upostag <* tab
             <*> xpostag <* tab
             <*> feats <* tab
             <*> dephead <* tab
             <*> deprel <* tab
             <*> deps <* tab
             <*> misc <* newline

emptyField :: Parser (Maybe a)
emptyField = do char '_'
                return Nothing

index :: Parser Index
index = do ix <- many1 digit
           return (read ix :: Index)

indexSep :: Parser IxSep
indexSep = choice [char '-', char '.']

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
             st  <- option [] $ char ':' *> many1 letter
             return (dep,st)
  where
    dep :: Parser Dep
    dep = liftM mkDep $ many1 letter

deps :: Parser Deps
deps = listP $ listPair ':' index deprel'

misc :: Parser Misc
misc = maybeEmpty stringWSpaces

---
-- utility parsers
litSpaces :: Parser ()
-- because spaces consumes \t and \n
litSpaces = skipMany $ char ' '

commentPair :: Parser Comment
commentPair =
  keyValue '=' (stringNot "=\n\t") (option [] stringWSpaces)

listPair :: Char -> Parser a -> Parser b -> Parser [(a, b)]
listPair sep p q = sepBy1 (keyValue sep p q) (char '|')

stringNot :: String -> Parser String
-- [ ] second litSpaces in symbol is redundant
stringNot s = symbol . many1 $ noneOf s

stringWSpaces :: Parser String
stringWSpaces = stringNot "\t\n"

stringWOSpaces :: Parser String
stringWOSpaces = stringNot " \t\n"

---
-- parser combinators
keyValue :: Char -> Parser a -> Parser b -> Parser (a, b)
keyValue sep p q = do key   <- p
                      optional $ char sep
                      value <- q
                      return (key, value)

symbol :: Parser a -> Parser a
symbol p = litSpaces *> p <* litSpaces

maybeEmpty :: Parser a -> Parser (Maybe a)
maybeEmpty p = emptyField <|> liftM Just p

listP :: Parser [a] -> Parser [a]
-- using a parser that returns a possibly empty list like sepBy and
-- many will return the correct result for the empty filed ('_'), but
-- will report it the same as any other syntax error
listP p = liftM (fromMaybe []) $ maybeEmpty p
