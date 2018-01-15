module Conllu.IO where

---
-- imports
import Conllu.Type
import Conllu.Parse (document)

import Prelude hiding (readFile)
import System.Directory
import System.Environment
import System.FilePath
import System.IO hiding (readFile)

import Text.Parsec.String

readFile :: FilePath -> IO Document
readFile f = do r <- parseFromFile document f
                case r of -- how to handle exceptions properly?
                  Left err  -> do {print err ; return $ Document f []}
                  Right ss  -> return $ Document (takeFileName f) ss

readDirectory :: FilePath -> IO [Document]
readDirectory d = do fs' <- listDirectory d
                     let fs = map (d </>) fs'
                     mapM readFile fs

readConllu :: FilePath -> IO [Document]
readConllu fp = do f <- doesFileExist fp
                   if' f (mapM readFile [fp]) $
                     do d <- doesDirectoryExist fp
                        if' d (readDirectory fp) (return [])

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
