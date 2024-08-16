{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Donation
  ( Donation(..)
  , createDonation
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import User (User)

data Donation = Donation
  { amount :: Double
  , purpose :: String
  , donor :: User
  } deriving (Show, Generic)

instance ToJSON Donation
instance FromJSON Donation

createDonation :: Double -> Text -> User -> Donation
createDonation amt prp usr = Donation { amount = amt, purpose = T.unpack prp, donor = usr }


