{-# LANGUAGE RecordWildCards #-}

module Report where

import Text.Regex.PCRE ((=~))
import Data.List (find, intercalate)
import Data.Text (unpack)
import Data.Map.Lazy (Map, empty, insertWith, toAscList)
import Data.Ratio (Rational)

import MyPrelude ((|>))
import Config (Config(Config), matchers, ignore)
import Transaction (Tx(Tx), getSum)

type Category = String

data Report = Report
    { totals :: Map Category Rational
    , uncategorized :: [Tx]
    , ignored :: [Tx]
    }

instance Show Report where
    show (Report {..}) = 
        [ [ "Ignored:" ]
        , map show ignored
        , [ "", "Categories:"]
        , totals |> toAscList |> map (\(cat, tot) -> (take 30 (cat ++ (replicate 30 ' '))) ++ ": " ++ (show tot))
        , [ "", "Uncategorized:" ]
        , map show uncategorized
        , [ "", "Sum uncategorized: " ++ (uncategorized |> map getSum |> sum |> show)]
        ] |> concat
          |> intercalate "\n"

makeReport :: Config -> [Tx] -> Report
makeReport config txs = foldr (updateReport config) (Report empty [] []) txs

updateReport :: Config -> Tx -> Report -> Report
updateReport config tx report = 
    let
        ignore = shouldIgnore config tx
        category = categorize config tx
    in
        case (ignore, category) of
            (True, _) -> report { ignored = tx:(ignored report) }
            (False, "") -> report { uncategorized = tx:(uncategorized report) }
            (False, _) -> report { totals = insertWith (+) category (getSum tx) (totals report)}

shouldIgnore :: Config -> Tx -> Bool
shouldIgnore (Config {..}) (Tx _ description _ _) =
        any ((unpack description) =~) ignore

categorize :: Config -> Tx -> Category
categorize (Config {..}) (Tx _ description _ _) = 
    maybe "" snd $ find (\(pattern, _) -> (unpack description) =~ pattern) matchers
