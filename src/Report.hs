{-# LANGUAGE RecordWildCards #-}

module Report where

import Text.Regex.PCRE ((=~))
import Data.List (find, intercalate)
import Data.Text (unpack)
import Data.Map.Lazy (Map, empty, insertWith, toAscList)
import Data.Ratio (Rational)

import MyPrelude ((|>), prettyAmount)
import Config (Config(Config), matchers, ignore)
import Transaction (Tx(Tx), getSum, getRatio, amountIn, amountOut)

type Category = String

data Report = Report
    { totals :: Map Category Rational
    , uncategorized :: [Tx]
    , ignored :: [Tx]
    , totalIn :: Rational
    , totalOut :: Rational
    }

instance Show Report where
    show (Report {..}) = 
        [ [ "Ignored:" ]
        , map show ignored
        , [ "", "Categories:"]
        , totals |> toAscList |> map (\(cat, tot) -> (take 30 (cat ++ ":" ++ (replicate 30 ' '))) ++ (prettyAmount tot))
        , [ "", "Uncategorized:" ]
        , map show uncategorized
        , [ "", "Sum uncategorized: " ++ (uncategorized |> map getSum |> sum |> prettyAmount)]
        , [ "", "Total in: " ++ (prettyAmount totalIn) ++ ", out: " ++ (prettyAmount totalOut)]
        ] |> concat
          |> intercalate "\n"

makeReport :: Config -> [Tx] -> String
makeReport config txs = show $ foldr (updateReport config) (Report empty [] [] 0 0) txs

updateReport :: Config -> Tx -> Report -> Report
updateReport config tx report = 
    let
        ignore = shouldIgnore config tx
        category = categorize config tx
    in
        case (ignore, category) of
            (True, _) -> report { ignored = tx:(ignored report) }
            (False, "") -> report
                { uncategorized = tx:(uncategorized report)
                , totalIn = (totalIn report) + (getRatio $ amountIn tx)
                , totalOut = (totalOut report) + (getRatio $ amountOut tx)
                }
            (False, _) -> report
                { totals = insertWith (+) category (getSum tx) (totals report)
                , totalIn = (totalIn report) + (getRatio $ amountIn tx)
                , totalOut = (totalOut report) + (getRatio $ amountOut tx)
                }

shouldIgnore :: Config -> Tx -> Bool
shouldIgnore (Config {..}) (Tx _ description _ _) =
        any ((unpack description) =~) ignore

categorize :: Config -> Tx -> Category
categorize (Config {..}) (Tx _ description _ _) = 
    maybe "" snd $ find (\(pattern, _) -> (unpack description) =~ pattern) matchers

printTxOf :: Category -> Config -> [Tx] -> String
printTxOf category config txs = 
    txs
        |> filter ((== category) . categorize config)
        |> map show
        |> intercalate "\n"
