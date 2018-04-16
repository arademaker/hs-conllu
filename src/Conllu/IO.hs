-- |
-- Module      :  Conllu.IO
-- Copyright   :  © 2018 bruno cuconato
-- License     :  LPGL-3
--
-- Maintainer  :  bruno cuconato <bcclaro+hackage@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines major IO functions.

module Conllu.IO where

---
-- imports
import           Conllu.Type
import           Conllu.Utils
import           Conllu.Parse
import           Conllu.Print
import           Conllu.Diff

import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
import qualified Text.Megaparsec as M


-- * read functions

---
-- ** readers using a customized parser
-- | these reader functions will read files using a customized
-- parser. you can build one with 'ParserC' and 'parserC'.
readConlluFileWith :: Parser Doc -> FilePath -> IO Doc
-- | reads a file with a customized parser.
readConlluFileWith p f = do
  d <- readFile f
  case parseConlluWith p f d of
    Left err -> putStr err *> return []
    Right d -> return d

readDirectoryWith :: Parser Doc -> FilePath -> IO [Doc]
-- | reads all the files in a directory as CoNLL-U files with a
-- customized parser.
readDirectoryWith p d = do fs' <- listDirectory d
                           let fs = map (d </>) fs'
                           mapM (readConlluFileWith p) fs

readConlluWith :: Parser Doc -> FilePath -> IO [Doc]
-- | reads a file or a directory as CoNLL-U files with a customized
-- parser.
readConlluWith p fp = do f <- doesFileExist fp
                         if' f (mapM (readConlluFileWith p) [fp]) $
                           do d <- doesDirectoryExist fp
                              if' d (readDirectoryWith p fp) (return [])

---
-- ** readers using default parsers
readConlluFile :: FilePath -> IO Doc
-- | reads a CoNLL-U file.
readConlluFile = readConlluFileWith document

readDirectory :: FilePath -> IO [Doc]
-- | reads all files in a directory as CoNLL-U files.
readDirectory = readDirectoryWith document

readConllu :: FilePath -> IO [Doc]
-- | reads a file or a directory as CoNLL-U files.
readConllu = readConlluWith document

---
-- * write
writeConlluFile :: FilePath -> Doc -> IO ()
-- | writes a CoNLL-U file to disk.
writeConlluFile fp = writeFile fp . printDoc

---
-- * print
readAndPrintConllu :: FilePath -> IO ()
-- | reads and prints the CoNLL-U files given.
readAndPrintConllu fp = do
  readConlluFile fp >>= putStr . printDoc
  return ()

---
-- * diff
diffConllu :: FilePath -> FilePath -> IO ()
-- | reads two CoNLL-U files and prints their diffs. this assumes
-- their sentences are paired.
diffConllu fp1 fp2 = do
  ss1 <- readConlluFile fp1
  ss2 <- readConlluFile fp2
  print . diffSs $ zip ss1 ss2
  return ()
