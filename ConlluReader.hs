module ConlluReader where

---
-- imports
import UD
import ConlluParser (document)

import System.Directory
import System.FilePath
import System.IO

import Text.Parsec.String

readConlluFile :: FilePath -> IO [Sentence]
readConlluFile f = do r <- parseFromFile document f
                      case r of
                        -- how to handle exceptions properly?
                        Left err  -> do {print err ; return []}
                        Right ss  -> return ss

readDirectory :: FilePath -> IO [Sentence]
readDirectory d = do fs' <- listDirectory d
                     let fs = map (d </>) fs'
                     ss <- mapM readConlluFile fs
                     return $ concat ss

readConllu :: FilePath -> IO [Sentence]
readConllu fp = do f <- doesFileExist fp
                   if' f (readConlluFile fp) $
                     do d <- doesDirectoryExist fp
                        if' d (readDirectory fp) (return [])

---
-- utility functions
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
