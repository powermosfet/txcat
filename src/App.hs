module App where

import Control.Monad.Reader (Reader, runReader)

import Config (Config)
import Options (Options)

type App = Reader (Config, Options) 

runApp :: App String -> (Config, Options) -> String
runApp = runReader 
