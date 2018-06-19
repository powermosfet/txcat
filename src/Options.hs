module Options where

import Options.Applicative
import Data.Monoid ((<>))
import System.FilePath ((</>))

data Options = Options
    { configPath :: FilePath
    , month :: Maybe Int
    , inputFiles :: [ FilePath ]
    }

options :: FilePath -> Parser Options
options homeDir = Options
    <$> strOption 
        ( long "config"
        <> metavar "PATH"
        <> help "path to config file"
        <> showDefault
        <> value (homeDir </> ".txcat.json")
        )
    <*> optional (option auto
            ( long "month"
              <> metavar "MM"
              <> help "only process given month"
            ))
    <*> some (argument str (metavar "INPUTFILES..."))

optionsWithInfo :: FilePath -> ParserInfo Options
optionsWithInfo homeDir = info (options homeDir <**> helper)
      ( fullDesc
     <> progDesc "Process and categorize CSV transactions from the bank"
     <> header "txcat - a regex-based transaction categorizer" )
