module Conllu.IO where

---
-- imports
import Conllu.Type
import Conllu.Utils
import Conllu.Parse
import Conllu.Print

import System.Directory
import System.Environment
import System.FilePath
import System.IO

import Text.Parsec.String

---
-- read

-- uses customized parser
readConlluFileWith :: Parser [Sentence] -> FilePath -> IO Document
readConlluFileWith p f = do
  r <- parseFromFile p f
  case r of
    Left err -> do
      print err
      return $ Document f []
    Right ss -> return $ Document (takeFileName f) ss

readDirectoryWith :: Parser [Sentence] -> FilePath -> IO [Document]
readDirectoryWith p d = do fs' <- listDirectory d
                           let fs = map (d </>) fs'
                           mapM (readConlluFileWith p) fs

readConlluWith :: Parser [Sentence] -> FilePath -> IO [Document]
readConlluWith p fp = do f <- doesFileExist fp
                         if' f (mapM (readConlluFileWith p) [fp]) $
                           do d <- doesDirectoryExist fp
                              if' d (readDirectoryWith p fp) (return [])

---
-- uses built-in parsers
readConlluFile :: FilePath -> IO Document
readConlluFile = readConlluFileWith document

readDirectory :: FilePath -> IO [Document]
readDirectory = readDirectoryWith document

readConllu :: FilePath -> IO [Document]
readConllu = readConlluWith document

---
-- write
writeConlluFile :: FilePath -> Document -> IO ()
writeConlluFile fp = writeFile fp . printDoc

---
-- main
main :: IO ()
main = do
  [input,output] <- getArgs
  d <- readConlluFile input
  writeConlluFile output d
  return ()
