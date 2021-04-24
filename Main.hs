{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Arrow (ArrowChoice ((|||)))
import Magic         (makeMagicSquare, printMagicSquare)
import MagicArgs     (Options (..), parseCmdArgs)
import System.IO     (hPutStrLn, stderr)

main :: IO ()
main = do
  Options{..} <- parseCmdArgs
  hPutStrLn stderr ||| printMagicSquare s $ makeMagicSquare a b c
