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
import Conllu.Type
import Conllu.Utils
import Conllu.Parse
import Conllu.Print

import System.Directory
import System.Environment
import System.FilePath
import System.IO
import qualified Text.Megaparsec as M


-- * read functions

---
-- ** readers using a customized parser
-- | these reader functions will read files using a customized parser
-- built with 'ParserC' and 'parserC'.
readConlluFileWith :: Parser [Sentence] -> FilePath -> IO Document
-- | reads a file with a customized parser.
readConlluFileWith p f = do
  d <- readFile f
  let r = M.parse p f d
  case r of
    Left err -> do
      putStr . M.parseErrorPretty $ err
      return $ Document f []
    Right ss -> return $ Document (takeFileName f) ss

readDirectoryWith :: Parser [Sentence] -> FilePath -> IO [Document]
-- | reads all the files in a directory as CoNLL-U files with a
-- customized parser.
readDirectoryWith p d = do fs' <- listDirectory d
                           let fs = map (d </>) fs'
                           mapM (readConlluFileWith p) fs

readConlluWith :: Parser [Sentence] -> FilePath -> IO [Document]
-- | reads a file or a directory as CoNLL-U files with a customized
-- parser.
readConlluWith p fp = do f <- doesFileExist fp
                         if' f (mapM (readConlluFileWith p) [fp]) $
                           do d <- doesDirectoryExist fp
                              if' d (readDirectoryWith p fp) (return [])

---
-- ** readers using default parsers
readConlluFile :: FilePath -> IO Document
-- | reads a CoNLL-U file.
readConlluFile = readConlluFileWith document

readDirectory :: FilePath -> IO [Document]
-- | reads all files in a directory as CoNLL-U files.
readDirectory = readDirectoryWith document

readConllu :: FilePath -> IO [Document]
-- | reads a file or a directory as CoNLL-U files.
readConllu = readConlluWith document

---
-- * write
writeConlluFile :: FilePath -> Document -> IO ()
-- | writes a CoNLL-U file to disk.
writeConlluFile fp = writeFile fp . printDoc

-- * print
readAndPrintConllu :: FilePath -> IO ()
-- | reads and prints the CoNLL-U files given.
readAndPrintConllu fp = do
  readConlluFile fp >>= putStr . printDoc
  return ()
