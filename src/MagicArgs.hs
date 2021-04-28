{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module MagicArgs (Options (..), parseCmdArgs) where

import Control.Arrow       (Arrow ((&&&)))
import Data.Char           (toLower, toUpper)
import Data.List           (intercalate)
import Magic               (SqStyle (..))
import Options.Applicative (Parser, ParserInfo, ReadM, argument, auto,
                            eitherReader, execParser, fullDesc,
                            header, help, helper, info, infoOption,
                            long, metavar, option, progDesc, short,
                            value)
import Text.Printf         (printf)

stylesLst :: [String]
stylesLst = map show [minBound :: SqStyle ..]

stylesStr :: String
stylesStr = intercalate ", " stylesLst

data Options = Options
  { a :: Int
  , b :: Int
  , c :: Int
  , s :: SqStyle }

parseCmdArgs :: IO Options
parseCmdArgs = execParser parseArgs

parseArgs :: ParserInfo Options
parseArgs = info
  (helper <*> ver <*> styles <*> opts)
  (fullDesc
    <> progDesc "Create a magic square of order three"
    <> header "magic-square")
  where
    ver, styles :: Parser (a -> a)
    ver    = infoOption "0.1.0.0" (short 'v' <> long "version" <> help "Show version")
    styles = infoOption stylesStr
      (long "styles-list" <> help "Print available styles")

opts :: Parser Options
opts = do
  a <- getArg "A" "A > 0"
  b <- getArg "B" "B > A && B /= 2 * A"
  c <- getArg "C" "C > B + A"
  s <- getStyle
  return Options{..}
  where
    getArg m h = argument auto (metavar m <> help h)
    getStyle   = option parseStyle (metavar "STYLE"
      <> long "style" <> short 's' <> value Default
      <> help "Magic square formatting style")

parseStyle :: ReadM SqStyle
parseStyle = eitherReader parse
  where
    parse :: String -> Either String SqStyle
    parse (id &&& titleCase -> (s, s')) = if elem s' stylesLst
      then Right (read s' :: SqStyle)
      else Left $ printf
        "cannot parse '%s'.\nAvailable styles:\n\t%s"
        s stylesStr
    titleCase (x:xs) = toUpper x : fmap toLower xs
