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

module Conllu.IO
(
  readAndPrintConllu,
  diffConllu,
  readDirectory,
  readConlluFile
)
where

---
-- imports
import Conllu.Type (Doc, Sent)
import Conllu.Utils (if')
import Conllu.Parse (Parser, sentence, parseConlluWith)
import Conllu.Print (printDoc)
import Conllu.Diff (diffSs, printDDiff)

import System.Directory (listDirectory, doesFileExist, doesDirectoryExist) --, getDirectoryContents)
import System.FilePath ((</>))  -- Combine two paths with a path separator

-- import Control.Monad (forM) --GVF added for RWH getRecursiveContents at end
import Data.Functor (($>))
import qualified Data.Text.IO as TIO
-- import qualified Data.Text as T

-- * read functions

---
-- ** readers using a customized parser
-- | these reader functions will read files using a customized
-- parser. you can build one with 'ParserC' and 'parserC'.
readConlluFileWith :: Parser Sent -> FilePath -> IO Doc
-- | reads a file with a customized parser.
readConlluFileWith p f = do
  ds <- TIO.readFile f
  case parseConlluWith p f ds of
    Left err -> putStr err $> []
    Right d -> return d

readDirectoryWith :: Parser Sent -> FilePath -> IO [Doc]
-- | reads all the files in a directory as CoNLL-U files with a
-- customized parser.
readDirectoryWith p d = do fs' <- listDirectory d
                           let fs = map (d </>) fs'
                           mapM (readConlluFileWith p) fs

readConlluWith :: Parser Sent -> FilePath -> IO [Doc]
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
readConlluFile = readConlluFileWith sentence

readDirectory :: FilePath -> IO [Doc]
-- | reads all files in a directory as CoNLL-U files.
readDirectory = readDirectoryWith sentence

readConllu :: FilePath -> IO [Doc]
-- | reads a file or a directory as CoNLL-U files.
readConllu = readConlluWith sentence

---
-- * write
writeConlluFile :: FilePath -> Doc -> IO ()
-- | writes a CoNLL-U file to disk.
writeConlluFile fp = TIO.writeFile fp . printDoc

---
-- * print
readAndPrintConllu :: FilePath -> IO ()
-- | reads and prints the CoNLL-U files given.
readAndPrintConllu fp = do
  readConlluFile fp >>= TIO.putStrLn . printDoc
  return ()

---
-- * diff
diffConllu :: FilePath -> FilePath -> IO ()
-- | reads two CoNLL-U files and prints their diffs. this assumes
-- their sentences are paired.
diffConllu fp1 fp2 = do
  ss1 <- readConlluFile fp1
  ss2 <- readConlluFile fp2
  print . printDDiff . diffSs $ zip ss1 ss2
  return ()
