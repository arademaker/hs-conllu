module Conllu.IO where

---
-- imports
import Conllu.Type
import qualified Conllu.Parse as P

import Prelude hiding (readFile)
import System.Directory
import System.Environment
import System.FilePath
import System.IO hiding (readFile)

import Text.Parsec.String

---
-- uses customized parser
readFileWith :: Parser [Sentence] -> FilePath -> IO Document
readFileWith p f = do
  r <- parseFromFile p f
  case r of
    Left err -> do print err
                   return $ Document f []
    Right ss -> return $ Document (takeFileName f) ss

readDirectoryWith :: Parser [Sentence] -> FilePath -> IO [Document]
readDirectoryWith p d = do fs' <- listDirectory d
                           let fs = map (d </>) fs'
                           mapM (readFileWith p) fs

readConlluWith :: Parser [Sentence] -> FilePath -> IO [Document]
readConlluWith p fp = do f <- doesFileExist fp
                         if' f (mapM (readFileWith p) [fp]) $
                           do d <- doesDirectoryExist fp
                              if' d (readDirectoryWith p fp) (return [])

---
-- uses built-in parsers
readFile :: FilePath -> IO Document
readFile = readFileWith P.document

readDirectory :: FilePath -> IO [Document]
readDirectory = readDirectoryWith P.document

readConllu :: FilePath -> IO [Document]
readConllu = readConlluWith P.document

---
-- utility functions
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

---
-- main
main :: IO ()
main = do
  [filename] <- getArgs
  ss <- readConllu filename
  putStr $ show ss
