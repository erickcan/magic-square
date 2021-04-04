module MagicArgs (Options (..), parseCmdArgs) where

import Options.Applicative (ParserInfo, argument, auto, execParser,
                            fullDesc, header, help, helper, info,
                            infoOption, long, metavar, progDesc)

data Options = Options
  { a :: Int
  , b :: Int
  , c :: Int }

parseCmdArgs :: IO Options
parseCmdArgs = execParser parseArgs

parseArgs :: ParserInfo Options
parseArgs = info
  (helper <*> ver <*> opts)
  (fullDesc
    <> progDesc "Create a magic square of order three"
    <> header "magic-square")
  where
    ver  = infoOption "0.1.0.0" (long "version" <> help "Show version")
    opts = Options
      <$> argument auto
        (help "A > 0" <> metavar "A")
      <*> argument auto
        (help "B > A && B /= 2 * A" <> metavar "B")
      <*> argument auto
        (help "C > B + A" <> metavar "C")
