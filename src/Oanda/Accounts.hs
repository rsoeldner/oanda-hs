{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}

module Oanda.Accounts where

import Data.Aeson
import qualified Data.Aeson.TH as A
import qualified Data.Text as T
import Servant.API

import Oanda.Internal

newtype AccountId = AccountId T.Text 
  deriving (FromJSON, ToJSON, Show, ToHttpApiData)

newtype AccountTag = AccountTag T.Text
  deriving (FromJSON, ToJSON, Show)

data Account
  = Account
  { accountId   :: AccountId
  , accountTags :: [AccountTag]
  } deriving Show

$(A.deriveJSON (options "account") ''Account)

newtype Accounts
  = Accounts
  { unAccounts :: [Account]
  } deriving Show

instance FromJSON Accounts where
  parseJSON = withObject "Accounts" $ \o -> Accounts <$> o .: "accounts"
