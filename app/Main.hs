module Main where

import Options.Applicative (execParser)
import System.Directory

import Lib (txCat)
import Options (optionsWithInfo)

main :: IO ()
main = getHomeDirectory >>= (execParser . optionsWithInfo) >>= txCat
