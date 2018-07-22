{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Transaction where

import Control.Applicative ((<*>))
import Data.Csv            ((.:))
import Data.List.Extra     (replace)
import Data.Text           (Text)
import Data.Text.Encoding  (decodeLatin1)
import Data.Time           (Day, toGregorian)
import Data.Time.Format    (parseTimeM, defaultTimeLocale)
import Data.Ratio          (Rational)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Csv              as Csv

import MyPrelude ((|>))

newtype CsvDay = CsvDay Day deriving (Show, Eq, Ord)

newtype Nok = Nok Rational

newtype Category = Category String deriving (Eq, Ord)

data Tx = Tx
    { txDate :: CsvDay
    , txDescription :: Text
    , txAmountIn :: Nok
    , txAmountOut :: Nok
    , txCategory :: Category
    , txIgnored :: Bool
    } 

getRatio :: Nok -> Rational
getRatio (Nok r) = r

instance Csv.FromField Nok where
    parseField bytes = 
        bytes
            |> BS.unpack
            |> (\s -> if s == "" then "0" else s)
            |> replace "," "."
            |> (read :: (String -> Double))
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
        <*> pure (Category "")
        <*> pure False

getSum :: Tx -> Rational
getSum (Tx _ _ (Nok i) (Nok o) _ _) = i + o

isMonth :: Maybe Int -> Tx -> Bool
isMonth mm (Tx (CsvDay day) _ _ _ _ _) =
    let
        (_, month, _) = toGregorian day
    in 
        case mm of
            (Just m) -> month == m
            Nothing -> True

isYear :: Maybe Integer -> Tx -> Bool
isYear yyyy (Tx (CsvDay day) _ _ _ _ _) =
    let
        (year, _, _) = toGregorian day
    in 
        case yyyy of
            (Just y) -> year == y
            Nothing -> True

isCategory :: Category -> Tx -> Bool
isCategory category = (== category) . txCategory

ledgerDate :: CsvDay -> String
ledgerDate (CsvDay day) = 
    let
        (yyyy, mm, dd) = toGregorian day
    in
        (show yyyy) ++ "/" ++ (show mm) ++ "/" ++ (show dd)
