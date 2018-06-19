{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Transaction where

import Control.Applicative ((<*>))
import Data.Csv            ((.:))
import Data.List.Extra     (replace)
import Data.Text           (Text, unpack)
import Data.Text.Encoding  (decodeLatin1)
import Data.Time           (Day, toGregorian)
import Data.Time.Format    (parseTimeM, defaultTimeLocale)
import Data.Ratio          (Rational)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Csv              as Csv

import MyPrelude ((|>))

newtype CsvDay = CsvDay Day deriving (Show)

newtype Nok = Nok Rational

data Tx = Tx
    { date :: CsvDay
    , description :: Text
    , amountIn :: Nok
    , amountOut :: Nok
    } 

getRatio :: Nok -> Rational
getRatio (Nok r) = r

readInt :: String -> Integer
readInt = read

instance Show Tx where
    show tx@(Tx (CsvDay day) descr _ _) = (show day) ++ "\t" ++ (take 50 ((unpack descr) ++ (replicate 50 ' '))) ++ "\t" ++ (show (getSum tx))

instance Csv.FromField Nok where
    parseField bytes = 
        bytes
            |> BS.unpack
            |> (\s -> if s == "" then "0" else s)
            |> replace "," "."
            |> readInt
            |> toRational
            |> Nok
            |> pure

instance Csv.FromField CsvDay where
    parseField bytes = 
        case CsvDay <$> parseTimeM False defaultTimeLocale "%d.%m.%Y" (BS.unpack bytes) of
            Just day -> pure day
            Nothing -> fail "Wrong date format"

instance Csv.FromNamedRecord Tx where
    parseNamedRecord r = Tx 
        <$> r .: "Dato" 
        <*> fmap decodeLatin1 (r .: "Beskrivelse")
        <*> r .: "Inn"
        <*> r .: "Ut"

getSum :: Tx -> Rational
getSum (Tx _ _ (Nok i) (Nok o)) = i + o

isMonth :: Maybe Int -> Tx -> Bool
isMonth mm (Tx (CsvDay day) _ _ _) =
    let
        (_, month, _) = toGregorian day
    in 
        case mm of
            (Just m) -> month == m
            _ -> True
