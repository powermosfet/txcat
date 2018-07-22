{-# LANGUAGE RecordWildCards #-}

module Report where

import Text.Regex.PCRE ((=~))
import Data.Text (unpack)
import Data.Map.Lazy (Map, empty, insertWith, toAscList)
import Data.Ratio (Rational)
import Control.Monad (foldM)
import Control.Monad.Reader (ask)

import App (App)
import MyPrelude ((|>), prettyAmount, rPad)
import Config (ignore)
import Transaction (Tx(Tx), Category(Category), CsvDay(CsvDay), txCategory, isCategory, getSum, getRatio, txAmountIn, txAmountOut)

data Report = Report
    { totals :: Map Category Rational
    , uncategorized :: [Tx]
    , ignored :: [Tx]
    , totalIn :: Rational
    , totalOut :: Rational
    }

txView :: Tx -> App String
txView tx@(Tx (CsvDay day) descr _ _ _) =
    return $ (show day) ++ " " ++ (rPad 50 (unpack descr)) ++ "\t" ++ (prettyAmount (getSum tx))

catView :: Category -> String
catView (Category c) = c

totalView :: (Category, Rational) -> String
totalView (cat, tot) = rPad 30 (catView cat) ++ prettyAmount tot

reportView :: Report -> App String
reportView (Report {..}) = do
    ignoredView <- mapM txView ignored
    let totalsView = totals |> toAscList |> map totalView
    uncategorizedView <- mapM txView uncategorized
    return $ [ ("Ignored:" : ignoredView)
             , ("\nCategories:" : totalsView)
             , ("\nUncategorized:" : uncategorizedView)
             , [ ("Sum uncategorized: " ++ (uncategorized |> map getSum |> sum |> prettyAmount))]
             , [ ("Total in: " ++ (prettyAmount totalIn) ++ ", out: " ++ (prettyAmount totalOut))]
             ] |> (unlines . concat)

makeReport :: [Tx] -> App Report
makeReport txs = 
    foldM updateReport (Report empty [] [] 0 0) txs

updateReport :: Report -> Tx -> App Report
updateReport report tx = do
    ignore <- shouldIgnore tx
    let category = txCategory tx
    return $ case (ignore, category) of
        (True, _) -> report { ignored = tx:(ignored report) }
        (False, Category "") -> report
            { uncategorized = tx:(uncategorized report)
            , totalIn = (totalIn report) + (getRatio $ txAmountIn tx)
            , totalOut = (totalOut report) + (getRatio $ txAmountOut tx)
            }
        (False, _) -> report
            { totals = insertWith (+) category (getSum tx) (totals report)
            , totalIn = (totalIn report) + (getRatio $ txAmountIn tx)
            , totalOut = (totalOut report) + (getRatio $ txAmountOut tx)
            }

shouldIgnore :: Tx -> App Bool
shouldIgnore  (Tx _ description _ _ _) = do
    (config, _) <- ask
    return $ any ((unpack description) =~) (ignore config)

printTxOf :: Category -> [Tx] -> App String
printTxOf category txs =
    unlines <$> (mapM txView (filter (isCategory category) txs))
