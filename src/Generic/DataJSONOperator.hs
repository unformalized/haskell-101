{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Generic.DataJSONOperator where

import GHC.Generics
import Data.String
import Data.ByteString
import Data.Aeson
import Control.Applicative

ind :: IsString a => a
ind = "{ \"name\": \"Flour\", \"quantity\": 250, \"measure\": \"gr\" }"

type Measure = String

data Ingredient = Ingredient
  { name :: String
  , quantity :: Int
  , measure  :: Maybe Measure
  } deriving (Show, Eq, Generic)

instance FromJSON Ingredient
instance ToJSON Ingredient

testDecodeJSON :: Maybe Object
testDecodeJSON = decode ind

value :: Ingredient
value = Ingredient { name = "Flour", quantity = 240, measure = Just "gr" }

testEncodeJSON = encode value
