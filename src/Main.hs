module Main where

import Conllu.IO (readAndPrintConllu)

import System.Environment

main :: IO ()
main = do
  (c:as) <- getArgs
  case c of
    "validate" -> readAndPrintConllu as
    _ -> return ()
