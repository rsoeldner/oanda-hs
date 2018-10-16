{-# language OverloadedStrings #-}

module Oanda.Internal where

import qualified Data.Text as T
import qualified Text.Read as T
import qualified Data.Aeson.TH as A
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Char as C

data AccountType = Production | Test

baseUrl :: AccountType -> T.Text
baseUrl Production = "https://api-fxtrade.oanda.com"
baseUrl Test       = "https://api-fxpractice.oanda.com"

options :: T.Text -> A.Options
options prefix
  = A.defaultOptions
  { A.fieldLabelModifier = normalize . drop (T.length prefix)
  , A.sumEncoding        = A.ObjectWithSingleField
  }
  where normalize (x:xs) = C.toLower x : xs


textRead :: (Read a) => Value -> Parser a
textRead = withText "text read" $ \t ->
  case T.readMaybe (T.unpack t) of
    Just v  -> pure v
    Nothing -> fail "Could not parse value"
   
