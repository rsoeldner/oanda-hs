{-# language TemplateHaskell #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}

module Oanda where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import Network.HTTP.Client.TLS (newTlsManager, tlsManagerSettings)



import Servant.API hiding (addHeader)
import Servant.Client
import Servant.Client.Core
import Data.Proxy
import Oanda.Accounts
import Oanda.Instruments
import Oanda.Internal

data AuthToken
type Auth = AuthenticatedRequest (AuthProtect AuthToken)

type AuthAPI = AccountAPI 

type AccountAPI = AuthProtect AuthToken :> "v3" :> "accounts" :> Get '[JSON] Accounts
             :<|> AuthProtect AuthToken :> "v3" :> "accounts" :> Capture "AccountId" AccountId :> "instruments" :> Get '[JSON] Instruments


newtype Token = Token T.Text

instance ToHttpApiData Token where
  toUrlPiece (Token tok) = "Bearer " <> tok

oandaAuth :: Token -> Auth
oandaAuth token = mkAuthenticatedRequest token (addHeader "Authorization")

type instance AuthClientData (AuthProtect AuthToken) = Token

api :: Proxy AuthAPI
api = Proxy



getAccounts'    :: Auth -> ClientM Accounts
getInstruments' :: Auth -> AccountId -> ClientM Instruments

getAccounts' :<|> getInstruments' = client api



getInstruments :: Auth -> AccountId -> ClientM [Instrument]
getInstruments auth accId = unInstruments <$> getInstruments' auth accId

getAccounts :: Auth -> ClientM [Account]
getAccounts auth = unAccounts <$> getAccounts' auth
