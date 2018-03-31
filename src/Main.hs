-- |
-- Module      :  Main
-- Copyright   :  © 2018 bruno cuconato
-- License     :  LPGL-3
--
-- Maintainer  :  bruno cuconato <bcclaro+hackage@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- the @hs-conllu@ executable.

module Main
  ( main )
where

import Conllu.IO (readAndPrintConllu)

import System.Environment

main :: IO ()
-- | @validate@ : read CoNLL-U file and print it to stdout. (this will
-- only apply the command to one file, so use your terminal's
-- completion mechanism to apply it to several files.
main = do
  (c:as) <- getArgs
  case c of
    "validate" -> mapM_ readAndPrintConllu as
    _ -> return ()
