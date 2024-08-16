{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Dashboard
  ( Dashboard(..)
  , initializeDashboard
  , updateDashboard
  , displayDashboard
  , saveDashboard
  ) where

import Data.Aeson (ToJSON, FromJSON, encode, decode)
import GHC.Generics (Generic)
import Donation (Donation(..), amount)
import System.IO (withFile, IOMode(..))
import qualified Data.ByteString.Lazy as B

data Dashboard = Dashboard
  { donations :: [Donation]
  , totalFunds :: Double
  , projectsFunded :: Int
  } deriving (Show, Generic)

instance ToJSON Dashboard
instance FromJSON Dashboard

-- Function to initialize a new dashboard
initializeDashboard :: Dashboard
initializeDashboard = Dashboard [] 0.0 0

-- Function to update the dashboard with a new donation
updateDashboard :: Dashboard -> Donation -> Dashboard
updateDashboard dashboard donation =
  dashboard { donations = donation : donations dashboard
            , totalFunds = totalFunds dashboard + amount donation
            , projectsFunded = projectsFunded dashboard + 1
            }

-- Function to display the current dashboard state
displayDashboard :: Dashboard -> IO ()
displayDashboard dashboard = do
  putStrLn $ "Dashboard updated: " ++ show dashboard

-- Function to save the dashboard state to a JSON file
saveDashboard :: Dashboard -> IO ()
saveDashboard dashboard =
  withFile "dashboard.json" WriteMode $ \h -> B.hPutStr h (encode dashboard)
