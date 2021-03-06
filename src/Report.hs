{-# LANGUAGE RecordWildCards #-}

module Report where

import Data.Text (unpack)
import Data.Map.Lazy (Map, empty, insertWith, toAscList)
import Data.Ratio (Rational)
import Control.Monad (foldM)
import Control.Monad.Reader (ask)

import App (App)
import MyPrelude ((|>), prettyAmount, rPad)
import Options (Options(Options), format, Format(OneLine, Ledger))
import Transaction (Tx(Tx), Category(Category), CsvDay(CsvDay), txCategory, getSum, getRatio, txAmountIn, txAmountOut, txIgnored, ledgerDate)

data Report = Report
    { totals :: Map Category Rational
    , uncategorized :: [Tx]
    , ignored :: [Tx]
    , totalIn :: Rational
    , totalOut :: Rational
    }

txView :: Tx -> App String
txView tx = do
    (_, Options { format = format }) <- ask
    return $ case format of
        OneLine -> txViewOneline tx
        Ledger acc -> txViewLedger acc tx
        
txViewOneline :: Tx -> String
txViewOneline tx@(Tx (CsvDay day) descr _ _ _ _) = 
    (show day) ++ " " ++ (rPad 50 (unpack descr)) ++ "\t" ++ (prettyAmount (getSum tx))

txViewLedger :: String -> Tx -> String
txViewLedger account tx@(Tx day descr _ _ (Category category) _) =
    "\n" ++ ledgerDate day ++ " " ++ (unpack descr) ++ 
        "\n\t" ++ account ++ "\t\t" ++ (prettyAmount (getSum tx)) ++ 
        "\n\t" ++ category ++ "\t\t"

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
    let ignore = txIgnored tx
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

printTx :: [Tx] -> App String
printTx txs =
    unlines <$> (mapM txView txs)
