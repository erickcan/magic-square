{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}

module Magic (makeMagicSquare, SqStyle (..), printMagicSquare) where

import Data.List   (intercalate)
import Text.Printf (printf)

data Row a = Row !a !a !a
  deriving (Show, Read, Eq, Functor, Foldable, Traversable)

data Square a = Square !a !a !a
  deriving (Show, Read, Eq, Functor, Foldable, Traversable)

newtype MagicSquare = MagicSquare {
  unMagic :: Square (Row Int)
}

instance Show MagicSquare where
  show = showMagicSquare Default

data SqStyle = Default | Minimal | Plus | Box | Array
  deriving (Show, Read, Enum, Bounded)

showMagicSquare :: SqStyle -> MagicSquare -> String
showMagicSquare s = fmt . formatMagSq (rowCol s)
  where
    fmt :: String -> String
    fmt = case s of
      Array -> (++"]]") . ("[["++) . concatMap (\case '\n' -> "]\n ["; x -> [x])
      _     -> id
    rowCol :: SqStyle -> (String, Maybe Char)
    rowCol = \case
      Default -> (" | ", Just '-')
      Minimal -> (" ", Nothing)
      Plus    -> (" + ", Just '+')
      Box     -> (" || ", Just '#')
      Array   -> (", ", Nothing)
    formatMagSq :: (String, Maybe Char) -> MagicSquare -> String
    formatMagSq (row, col) = displaySquare col . fmtSquare row . unMagic

printMagicSquare :: SqStyle -> MagicSquare -> IO ()
printMagicSquare = (.showMagicSquare) (putStrLn.)

justifyRight :: Int -> String -> String
justifyRight i t
  | lenT >= i = t
  | otherwise = replicate (i - lenT) ' ' ++ t
  where lenT  = length t

fmtRow :: String -> Int -> Row String -> String
fmtRow sep n = addSep . justify n . words . addSpace
  where
    addSpace = foldr1 $ (++) . (++" ")
    justify  = fmap . justifyRight
    addSep   = intercalate sep

fmtSquare :: Show a => String -> Square (Row a) -> Square String
fmtSquare sep sq = fmtRow sep (wideLen sq) <$> sqRowStr sq
  where
    sqRowStr = fmap . fmap $ show
    wideLen  = maxFmap widestElem
      where
        maxFmap fn = maximum . fmap fn
        widestElem = maxFmap (length . show)

displaySquare :: Maybe Char -> Square String -> String
displaySquare c sq = foldr1 sep sq
  where
    lengthSq = fmap length sq
    lengthN  = (\(Square _ x _) -> x) lengthSq
    sepStr   = case c of
      Just ch -> replicate lengthN ch
      Nothing -> []
    append x = (++) (x ++ "\n")
    sep x y  = case sepStr of
      [] -> append x y
      _  -> append (append x sepStr) y

makeMagicSquare :: Int -> Int -> Int -> Either String MagicSquare
makeMagicSquare a b c = if isValid
  then Right $ MagicSquare $ Square
    (Row (c - b)       (c + a + b)   (c - a))
    (Row (c - (a - b)) c             (c + (a - b)))
    (Row (c + a)       (c - (a + b)) (c + b))
  else Left $ printf
    "invalid input: {a = %d, b = %d, c = %d}\n\
    \\nenter a, b, and c such that:\n\
    \  a > 0\n\
    \  b > a && b /= 2 * a\n\
    \  c > b + a" a b c
  where
    isValid = a > 0 && b > a && b /= 2 * a && c > b + a
