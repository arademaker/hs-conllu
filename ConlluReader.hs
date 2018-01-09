module ConlluReader where

---
-- imports
import UD
import ConlluParser (document)

import Prelude hiding (readFile)
import System.Directory
import System.Environment
import System.FilePath
import System.IO hiding (readFile)

import Text.Parsec.String

readFile :: FilePath -> IO [Sentence]
readFile f = do r <- parseFromFile document f
                case r of -- how to handle exceptions properly?
                  Left err  -> do {print err ; return []}
                  Right ss  -> return ss

readDirectory :: FilePath -> IO [Sentence]
readDirectory d = do fs' <- listDirectory d
                     let fs = map (d </>) fs'
                     ss <- mapM readFile fs
                     return $ concat ss

readConllu :: FilePath -> IO [Sentence]
readConllu fp = do f <- doesFileExist fp
                   if' f (readFile fp) $
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
