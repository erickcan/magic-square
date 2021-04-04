{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Magic (makeMagicSquare) where

import Data.List   (intercalate)
import Text.Printf (printf)

data Row a = Row !a !a !a
  deriving (Read, Eq, Functor, Foldable, Traversable)

instance Show a => Show (Row a) where
  show = fmtRow 0 . fmap show

data Square a = Square !a !a !a
  deriving (Read, Eq, Functor, Foldable, Traversable)

instance Show a => Show (Square a) where
  show = ("Square"++) . concatMap ((" "++) . show)

instance {-# OVERLAPPING #-} Show a => Show (Square (Row a)) where
  show = displaySquare . fmtSquare

newtype MagicSquare = MagicSquare {
  unMagic :: Square (Row Int)
}

instance Show MagicSquare where
  show = show . unMagic

justifyRight :: Int -> String -> String
justifyRight i t
  | lenT >= i = t
  | otherwise = replicate (i - lenT) ' ' ++ t
  where lenT  = length t

fmtRow :: Int -> Row String -> String
fmtRow n = addSep . justify n . words . addSpace
  where
    addSpace = foldr1 $ (++) . (++" ")
    justify  = fmap . justifyRight
    addSep   = intercalate " | "

fmtSquare :: Show a => Square (Row a) -> Square String
fmtSquare sq = fmtRow (wideLen sq) <$> sqRowStr sq
  where
    sqRowStr = fmap . fmap $ show
    wideLen  = maxFmap widestElem
      where
        maxFmap fn = maximum . fmap fn
        widestElem = maxFmap (length . show)

displaySquare :: Square String -> String
displaySquare sq = foldr1 sep sq
  where
    lengthSq = fmap length sq
    lengthN  = (\(Square _ x _) -> x) lengthSq
    sepStr   = replicate lengthN '-'
    append x = (++) (x ++ "\n")
    sep x y  = append (append x sepStr) y

makeMagicSquare :: Int -> Int -> Int -> Either String MagicSquare
makeMagicSquare a b c = if isValid
  then Right $ MagicSquare $ Square
    (Row (c - b)       (c + a + b)   (c - a)      )
    (Row (c - (a - b)) c             (c + (a - b)))
    (Row (c + a)       (c - (a + b)) (c + b)      )
  else Left $ printf
    "invalid input: {a = %d, b = %d, c = %d}\n\
    \\nenter a, b, and c such that:\n\
    \  a > 0\n\
    \  b > a && b /= 2 * a\n\
    \  c > b + a" a b c
  where
    isValid = a > 0 && b > a && b /= 2 * a && c > b + a
