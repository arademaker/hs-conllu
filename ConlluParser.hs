module ConlluParser where

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

-- TODO: check what's the purpose of the '_' in this MISC field:
-- '_|SpaceAfter=Yes' should it be allowed?

-- TODO: create non validating parser: count 10 (stringNot "\t\n" <* tab)

--}

---
-- imports

-- lib imports
import UD

-- stdlib
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.IO

-- hackage
import Text.Parsec hiding (token,tokens)
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char

---
-- conllu parsers
document :: Parser [Sentence]
document = do endBy1 sentence blankLine <* eof

blankLine :: Parser ()
blankLine = do litSpaces <* newline -- spaces shouldn't exist, but no
                                    -- problem being lax here (I think)

sentence :: Parser Sentence
sentence = do liftM2 Sentence (many comment) (many1 token)

comment :: Parser Comment
comment = do char '#'
             commentPair <* newline

token :: Parser Token
-- check liftM5 and above for no variable parsing, see
-- graham's book
token = do mkToken <$> index
             <*> optionMaybe indexSep
             <*> optionMaybe index <* tab
             <*> formP <* tab
             <*> lemmaP <* tab
             <*> upostagP <* tab
             <*> xpostagP <* tab
             <*> featsP <* tab
             <*> depheadP <* tab
             <*> deprelP <* tab
             <*> depsP <* tab
             <*> miscP <* newline

emptyField :: Parser (Maybe a)
emptyField = do char '_'
                return Nothing

index :: Parser Index
index = do ix <- many1 digit
           return (read ix :: Index)

indexSep :: Parser IxSep
indexSep = do choice [char '-', char '.']

formP :: Parser Form
formP = do maybeEmpty stringWSpaces

lemmaP :: Parser Lemma
lemmaP = do maybeEmpty stringWSpaces

upostagP :: Parser PosTag
upostagP = do maybeEmpty upostagP'
  where
    upostagP' :: Parser Pos
    upostagP' = do liftM mkPos stringWOSpaces

xpostagP :: Parser Xpostag
xpostagP = do maybeEmpty stringWOSpaces

featsP :: Parser Feats
featsP = do listP $ listPair '=' (stringNot "=") (stringNot "\t|")

depheadP :: Parser Dephead
depheadP = do maybeEmpty $ symbol index

deprelP :: Parser DepRel
deprelP = do maybeEmpty $ deprelP'

deprelP' :: Parser (Dep, Subtype)
deprelP' = do dep <- depP
              st  <- option [] $ char ':' *> many1 letter
              return (dep,st)
  where
    depP :: Parser Dep
    depP = do liftM mkDep $ many1 letter

depsP :: Parser Deps
depsP = do listP $ listPair ':' index deprelP'

miscP :: Parser Misc
miscP = do maybeEmpty stringWSpaces

---
-- utility parsers
litSpaces :: Parser ()
-- because spaces consumes \t and \n
litSpaces = do skipMany $ char ' '

commentPair :: Parser Comment
commentPair = do
  keyValue '=' (stringNot "=\n\t") (option [] stringWSpaces)

listPair :: Char -> Parser a -> Parser b -> Parser [(a, b)]
listPair sep p q = do
  sepBy1 (keyValue sep p q) (char '|')

stringNot :: String -> Parser String
-- [ ] second litSpaces in symbol is redundant
stringNot s = do symbol . many1 $ noneOf s

stringWSpaces :: Parser String
stringWSpaces = do stringNot "\t\n"

stringWOSpaces :: Parser String
stringWOSpaces = do stringNot " \t\n"

---
-- parser combinators
keyValue :: Char -> Parser a -> Parser b -> Parser (a, b)
keyValue sep p q = do key   <- p
                      optional $ char sep
                      value <- q
                      return (key, value)

symbol :: Parser a -> Parser a
symbol p = do litSpaces *> p <* litSpaces

maybeEmpty :: Parser a -> Parser (Maybe a)
maybeEmpty p = do emptyField <|> liftM Just p

listP :: Parser [a] -> Parser [a]
-- using a parser that returns a possibly empty list like sepBy and
-- many will return the correct result for the empty filed ('_'), but
-- will report it the same as any other syntax error
listP p = do liftM (fromMaybe []) $ maybeEmpty p
