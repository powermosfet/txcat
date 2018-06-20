{-# LANGUAGE RecordWildCards #-}

module Lib where

import Control.Monad (mapM)
import Data.Aeson (eitherDecode)
import qualified Data.Vector as Vector
import Data.Char (ord)
import Data.Csv
import qualified Data.ByteString.Lazy as BS

import MyPrelude ((|>))
import Options (Options(Options), month, printCategory)
import Transaction (Tx, isMonth)
import Config (Config)
import Report (makeReport, printTxOf)

csvParsingOptions :: DecodeOptions
csvParsingOptions = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }

txCat :: Options.Options -> IO ()
txCat options@(Options aConfigPath _ _ theInputFiles) = do
    configFile <- BS.readFile aConfigPath
    fileContents <- mapM BS.readFile theInputFiles
    either putStrLn putStrLn $ getOutput options configFile fileContents

getOutput :: Options.Options -> BS.ByteString -> [BS.ByteString] -> Either String String
getOutput options configFile inputFiles = do
    config <- parseConfig configFile
    txs <- parseAllCsvs inputFiles
    filteredTxs <- return $ filter (isMonth (month options)) txs
    case (printCategory options) of
        Just category -> return $ printTxOf category config filteredTxs
        Nothing -> return $ makeReport config filteredTxs
        

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

