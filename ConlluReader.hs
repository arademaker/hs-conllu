module ConlluReader where

---
-- imports
import UD
from ConlluParser import document

import System.IO
import System.Environment
import System.Directory
import System.Exit

import Text.Parsec.String

readConlluFile :: FilePath -> IO [Sentence]
readConlluFile f = do parseFromFile document f >>= either report return

readDirectory :: FilePath -> IO [Sentence]
readDirectory d = do let fs = listDirectory d
                     concatMap readConlluFile fs

readConllu :: FilePath -> IO [Sentence]
readConllu fp = do return $ case fp of
                              doesFileExist fp -> readConlluFile fp
                              doesDirectoryExist fp -> readDirectory fp
                              _ -> []

---
-- utility functions
report :: (Either ParseError a) -> IO ()
report err = do hPutStrLn stderr $ "Error: " ++ show err
                exitFailure