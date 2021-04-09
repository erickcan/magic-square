module MagicArgs (Options (..), parseCmdArgs) where

import Magic               (SqStyle (..))
import Options.Applicative (Alternative ((<|>)), Parser, ParserInfo,
                            argument, auto, execParser, flag, flag',
                            fullDesc, header, help, helper, info,
                            infoOption, long, metavar, progDesc,
                            short)

data Options = Options
  { a :: Int
  , b :: Int
  , c :: Int
  , s :: SqStyle }

parseCmdArgs :: IO Options
parseCmdArgs = execParser parseArgs

parseArgs :: ParserInfo Options
parseArgs = info
  (helper <*> ver <*> opts)
  (fullDesc
    <> progDesc "Create a magic square of order three"
    <> header "magic-square")
  where
    ver = infoOption "0.1.0.0" (short 'v' <> long "version" <> help "Show version")

opts :: Parser Options
opts = Options <$> getA <*> getB <*> getC <*> getStyle

getA, getB, getC :: Parser Int
getA = argument auto (help "A > 0" <> metavar "A")
getB = argument auto (help "B > A && B /= 2 * A" <> metavar "B")
getC = argument auto (help "C > B + A" <> metavar "C")

getStyle :: Parser SqStyle
getStyle = flag Default Default
  (long "default-style" <> help "Use the default style for the square")
  <|> styleFlag Minimal "minimal-style" "Use a minimal style for the square"
  <|> styleFlag Plus    "plus-style"    "Use plus signs as separators"
  <|> styleFlag Box     "box-style"     "Use || and # as column and row separators"
  where
    styleFlag :: SqStyle -> String -> String -> Parser SqStyle
    styleFlag st l h = flag' st (long l <> help h)
