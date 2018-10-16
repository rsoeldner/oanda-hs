{-# language GeneralizedNewtypeDeriving #-}
{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Oanda.Instruments where

import qualified Data.Text as T
import Data.Aeson

--import Oanda.Accounts
import Oanda.Internal
import Servant.API

newtype InstrumentName = InstrumentName T.Text
  deriving (Show, ToJSON, FromJSON, ToHttpApiData)

data Instrument
  = Instrument
  { instrumentDisplayName                 :: T.Text
  , instrumentDisplayPrecision            :: Int
  , instrumentMarginRate                  :: Float
  , instrumentMaximumOrderUnits           :: Int
  , instrumentMaximumPositionSize         :: Int
  , instrumentMaximumTrailingStopDistance :: Float
  , instrumentMinimumTradeSize            :: Int
  , instrumentMinimumTrailingStopDistance :: Float
  , instrumentName                        :: InstrumentName
  , instrumentPipLocation                 :: Int
  , instrumentTradeUnitsPrecision         :: Int
  , instrumentType                        :: T.Text
  } deriving Show

instance FromJSON Instrument where
  parseJSON = withObject "Instrument" $ \o -> do
    instrumentDisplayName                 <- o .: "displayName"
    instrumentDisplayPrecision            <- o .: "displayPrecision"
    instrumentMarginRate                  <- o .: "marginRate" >>= textRead
    instrumentMaximumOrderUnits           <- o .: "maximumOrderUnits" >>= textRead
    instrumentMaximumPositionSize         <- o .: "maximumPositionSize" >>= textRead
    instrumentMaximumTrailingStopDistance <- o .: "maximumTrailingStopDistance" >>= textRead
    instrumentMinimumTradeSize            <- o .: "minimumTradeSize" >>= textRead
    instrumentMinimumTrailingStopDistance <- o .: "minimumTrailingStopDistance" >>= textRead
    instrumentPipLocation                 <- o .: "pipLocation" 
    instrumentName                        <- o .: "name"
    instrumentTradeUnitsPrecision         <- o .: "tradeUnitsPrecision"
    instrumentType                        <- o .: "type"
    return $ Instrument{..}


newtype Instruments
  = Instruments
  { unInstruments :: [Instrument]
  } deriving Show

instance FromJSON Instruments where
  parseJSON = withObject "Instruments" $ \o -> Instruments <$> o .: "instruments"

