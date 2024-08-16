{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DonationType
  ( Donation(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import SmartContract (SmartContract)

-- Donation data type
data Donation = Donation
  { donationId :: Int
  , donationAmount :: Double
  , donationPurpose :: Text
  , donationExpenditure :: Double
  , donationProgress :: Text
  , donationSmartContract :: SmartContract
  } deriving (Show, Generic)

instance FromJSON Donation
instance ToJSON Donation
