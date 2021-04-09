module Main where

import Control.Arrow (ArrowChoice ((|||)))
import Magic         (makeMagicSquare, printMagicSquare)
import MagicArgs     (Options (..), parseCmdArgs)
import System.IO     (hPutStrLn, stderr)

main :: IO ()
main = do
  (Options x y z f) <- parseCmdArgs
  hPutStrLn stderr ||| printMagicSquare f $ makeMagicSquare x y z
