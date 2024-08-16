{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SmartContract where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- SmartContract data type
data SmartContract = SmartContract
  { contractName :: Text
  , contractTerms :: Text
  } deriving (Show, Generic)

instance FromJSON SmartContract
instance ToJSON SmartContract

-- Function to execute a smart contract
executeSmartContract :: SmartContract -> IO ()
executeSmartContract contract = do
  putStrLn $ "Executing smart contract: " ++ show (contractName contract)
  putStrLn $ "Terms: " ++ show (contractTerms contract)
