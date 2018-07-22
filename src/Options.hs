module Options where

import Options.Applicative
import Data.Monoid ((<>))
import System.FilePath ((</>))

data Format 
    = OneLine
    | Ledger String
        deriving (Show, Read)

data Options = Options
    { configPath :: FilePath
    , month :: Maybe Int
    , year :: Maybe Integer
    , printCategory :: Maybe String
    , inputFiles :: [ FilePath ]
    , format :: Format
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
              <> short 'm'
              <> metavar "MM"
              <> help "only process transactions from month MM"
            ))
    <*> optional (option auto
            ( long "year"
              <> short 'y'
              <> metavar "YYYY"
              <> help "only process transactions from year YYYY"
            ))
    <*> optional (strOption
            ( long "print-category"
              <> short 'p'
              <> help "Print all transactions of a given category"
            ))
    <*> some (argument str (metavar "INPUTFILES..."))
    <*> option auto
        ( long "format"
        <> metavar "FORMAT"
        <> help "output format for transactions"
        <> showDefault
        <> value OneLine
        )

optionsWithInfo :: FilePath -> ParserInfo Options
optionsWithInfo homeDir = info (options homeDir <**> helper)
      ( fullDesc
     <> progDesc "Process and categorize CSV transactions from the bank"
     <> header "txcat - a regex-based transaction categorizer" )
