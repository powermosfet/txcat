{-# LANGUAGE RecordWildCards #-}

module Lib where

import Control.Monad (mapM)
import Control.Monad.Reader (ask)
import Text.Regex.PCRE ((=~))
import Data.Aeson (eitherDecode)
import qualified Data.Vector as Vector
import Data.Char (ord)
import Data.List (find)
import Data.Csv
import GHC.Exts (sortWith)
import Data.Text (unpack)
import qualified Data.ByteString.Lazy as BS

import MyPrelude ((|>))
import Options (Options(Options), month, year, printCategory, printAll)
import Transaction (Tx, Category(Category), isMonth, isYear, txDate, txCategory, txDescription, isCategory)
import Config (Config(Config), matchers, defaultCategory)
import Report (makeReport, reportView, printTx)
import App (App, runApp)

csvParsingOptions :: DecodeOptions
csvParsingOptions = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }

txCat :: Options.Options -> IO ()
txCat options@(Options aConfigPath _ _ _ _ theInputFiles _) = do
    configFile <- BS.readFile aConfigPath
    fileContents <- mapM BS.readFile theInputFiles
    either putStrLn putStrLn $ tryReadFiles options configFile fileContents

tryReadFiles :: Options.Options -> BS.ByteString -> [BS.ByteString] -> Either String String
tryReadFiles options configFile inputFiles = do
    config <- parseConfig configFile
    txs <- parseAllCsvs inputFiles
    return $ runApp (app txs) (config, options)

categorize :: Tx -> App Tx
categorize tx = do
    (Config { matchers = matchers, defaultCategory = mDefaultCategory }, _) <- ask
    let defaultCategory = maybe "" id mDefaultCategory
    let descr = (unpack (txDescription tx))
    let theCategory = maybe defaultCategory snd $ find (\(pattern, _) -> descr =~ pattern) matchers
    return $ tx { txCategory = Category theCategory }

app :: [Tx] -> App String
app txs = do
    (_, Options {..}) <- ask
    txs' <- txs
        |> filter (isMonth month)
        |> filter (isYear year)
        |> sortWith txDate
        |> mapM categorize
    case (printCategory, printAll) of
        (Just category, _) -> printTx (filter (isCategory (Category category)) txs')
        (Nothing, True) -> printTx txs'
        (Nothing, False) -> makeReport txs' >>= reportView
        

parseConfig :: BS.ByteString -> Either String Config
parseConfig configFileContent = eitherDecode configFileContent

parseAllCsvs :: [BS.ByteString] -> Either String [Tx]
parseAllCsvs files =
    files
        |> map parseSingleCsv
        |> sequence
        |> fmap concat

parseSingleCsv :: BS.ByteString -> Either String [Tx]
parseSingleCsv contents = 
    contents
        |> decodeByNameWith csvParsingOptions 
        |> fmap (Vector.toList . snd)

