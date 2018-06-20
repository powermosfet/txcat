{-# LANGUAGE RecordWildCards #-}

module Lib where

import Control.Monad (mapM)
import Data.Aeson (eitherDecode)
import qualified Data.Vector as Vector
import Data.Char (ord)
import Data.Csv
import qualified Data.ByteString.Lazy as BS

import Control.Applicative ((<*>))

import MyPrelude ((|>))
import Options (Options(Options))
import Transaction (Tx, isMonth)
import Config (Config)
import Report (makeReport, Category)

csvParsingOptions :: DecodeOptions
csvParsingOptions = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }

txCat :: Options.Options -> IO ()
txCat (Options aConfigPath mm printCategory theInputFiles) = do
    configFile <- BS.readFile aConfigPath
    fileContents <- mapM BS.readFile theInputFiles
    let eConfig = parseConfig configFile 
    let eTxs = parseAllCsvs fileContents
    let eFilteredTxs = fmap (filter (isMonth mm)) eTxs
    let output = case printCategory of
        Just category -> (printTxOf category) <$> eConfig <*> eFilteredTxs
        Nothing -> makeReport <$> eConfig <*> eFilteredTxs
    either putStrLn putStrLn output

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

