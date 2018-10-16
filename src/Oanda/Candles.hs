{-# language OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RecordWildCards #-}

module Oanda.Candles where

import Servant.API
import Data.Aeson
import Data.Time (UTCTime)
import qualified Data.Text as T

import Oanda.Internal

data CandleType
  = MidpointCandles
  | AskCandles
  | BidCandles

instance ToHttpApiData CandleType where
  toQueryParam MidpointCandles = T.pack "M"
  toQueryParam AskCandles = T.pack "A"
  toQueryParam BidCandles = T.pack "B"

data CandleGranularity
  = S5
  | S10
  | S15
  | S30
  | M1
  | M2
  | M4
  | M5
  | M10
  | M15
  | M30
  | H1
  | H2
  | H3
  | H6
  | H8
  | H12
  | D
  | W
  | M
 deriving (Show)

instance ToHttpApiData CandleGranularity where
  toQueryParam = T.pack . show


data Candle
  = Candle
  { candleTime :: UTCTime
  , candleAsk :: Maybe OHLC
  , candleBid :: Maybe OHLC
  , candleMid :: Maybe OHLC
  , candleVolume :: Int
  , candleComplete :: Bool
  } deriving Show


instance FromJSON Candle where
  parseJSON = withObject "Candle" $ \o -> do
    candleTime <- o .: "time"
    candleAsk <- o .:? "ask"
    candleBid <- o .:? "bid"
    candleMid <- o .:? "mid"
    candleVolume <- o .: "volume"
    candleComplete <- o .: "complete"
    return Candle{..}

newtype Candles
  = Candles
  { unCandles :: [Candle]
  }

instance FromJSON Candles where
  parseJSON = withObject "Candles" $ \o -> Candles <$> o .: "candles" 

data OHLC
  = OHLC
  { ohlcOpen :: Float
  , ohlcHigh :: Float
  , ohlcLow  :: Float
  , ohlcClose:: Float
  } deriving Show

instance FromJSON OHLC where
  parseJSON = withObject "ohlc" $ \o -> do
    ohlcOpen <- o .: "o" >>= textRead
    ohlcHigh <- o .: "h" >>= textRead
    ohlcLow  <- o .: "l" >>= textRead
    ohlcClose<- o .: "c" >>= textRead
    return OHLC{..}
