module Main where

import Control.Arrow (ArrowChoice ((|||)))
import Magic         (makeMagicSquare)
import MagicArgs     (Options (..), parseCmdArgs)
import System.IO     (hPutStrLn, stderr)

main :: IO ()
main = do
  (Options x y z) <- parseCmdArgs
  hPutStrLn stderr ||| print $ makeMagicSquare x y z
