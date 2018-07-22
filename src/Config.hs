{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

type Pattern = String

type Category = String

data Config = Config
    { ignore :: [ Pattern ]
    , matchers :: [ ( Pattern, Category ) ]
    , month :: Maybe Int
    , defaultCategory :: Maybe String
    } deriving (Generic, Show)

instance FromJSON Config 
