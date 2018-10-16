{-# language TemplateHaskell #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}

module Oanda where

import qualified Data.Text as T
import Network.HTTP.Client.TLS (newTlsManager, tlsManagerSettings)

import Servant.API hiding (addHeader)
import Servant.Client
import Servant.Client.Core
import Data.Proxy
import Data.Time

import Oanda.Accounts
import Oanda.Instruments
import Oanda.Candles
import Oanda.Internal

data AuthToken
type Auth = AuthenticatedRequest (AuthProtect AuthToken)

type AuthAPI = AccountAPI
          :<|> InstrumentAPI 

type AccountAPI = AuthProtect AuthToken :> "v3" :> "accounts" :> Get '[JSON] Accounts
             :<|> AuthProtect AuthToken :> "v3" :> "accounts" :> Capture "AccountId" AccountId :> "instruments" :> Get '[JSON] Instruments

type InstrumentAPI = AuthProtect AuthToken :> "v3" :> "instruments" 
  :> Capture "InstrumentName" InstrumentName :> "candles"
  :> QueryParam "price" CandleType
  :> QueryParam "from" UTCTime
  :> QueryParam "to" UTCTime
  :> QueryParam "granularity" CandleGranularity 
  :> Get '[JSON] Candles 

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
getCandles'     :: Auth -> InstrumentName -> Maybe CandleType -> Maybe UTCTime -> Maybe UTCTime -> Maybe CandleGranularity -> ClientM Candles

(getAccounts' :<|> getInstruments') :<|> getCandles' = client api



getInstruments :: Auth -> AccountId -> ClientM [Instrument]
getInstruments auth accId = unInstruments <$> getInstruments' auth accId

getAccounts :: Auth -> ClientM [Account]
getAccounts auth = unAccounts <$> getAccounts' auth

getCandles :: Auth -> InstrumentName -> Maybe CandleType -> Maybe UTCTime -> Maybe UTCTime -> Maybe CandleGranularity -> ClientM [Candle]
getCandles auth instrument candleType from to granularity = unCandles <$> getCandles' auth instrument candleType from to granularity

