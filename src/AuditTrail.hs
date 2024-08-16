{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AuditTrail
  ( AuditTrail(..)
  , Transaction(..)
  , User(..)
  , Allocation(..)
  , Project(..)
  , Impact(..)
  , loadAuditTrail
  , saveAuditTrail
  , addTransaction
  , generateReport
  , generateAnalytics
  , verifyProject
  , approveProject
  , calculateImpact
  , generateImpactReport
  , createTransaction
  , submitTransaction
  ) where

import CardanoTransactions
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import GHC.Generics (Generic)
import System.IO (withFile, IOMode(..), openFile, hClose, Handle)
import qualified Data.ByteString.Lazy as B
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (renameFile)
import System.FilePath ((<.>))
import Data.List (groupBy, sortOn)
import Data.Function (on)
import Data.Time.Format (defaultTimeLocale, formatTime)

-- Allocation data type to specify fund distribution
data Allocation = Allocation
  { project :: String
  , amount :: Double
  } deriving (Show, Generic)

instance ToJSON Allocation
instance FromJSON Allocation

-- User data type
data User = User
  { userId :: Int
  , userName :: Text
  , userEmail :: Text
  , userVerified :: Bool
  } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

-- Project data type
data Project = Project
  { projectId :: Int
  , projectName :: String
  , projectDescription :: String
  , projectVerified :: Bool
  , projectApproved :: Bool
  } deriving (Show, Generic)

instance ToJSON Project
instance FromJSON Project

-- Impact data type
data Impact = Impact
  { impactId :: Int
  , impactDescription :: String
  , impactValue :: Double
  , associatedProjectId :: Int
  } deriving (Show, Generic)

instance ToJSON Impact
instance FromJSON Impact

-- Updated Transaction data type to include allocations
data Transaction = Transaction
  { transactionId :: Int
  , transactionTime :: UTCTime
  , transactionAmount :: Double
  , transactionPurpose :: String
  , transactionDonor :: Text
  , transactionAllocations :: [Allocation]
  } deriving (Show, Generic)

instance ToJSON Transaction
instance FromJSON Transaction

data AuditTrail = AuditTrail
  { transactions :: [Transaction]
  , projects :: [Project]
  , impacts :: [Impact]
  } deriving (Show, Generic)

instance ToJSON AuditTrail
instance FromJSON AuditTrail

-- Function to load the audit trail from a JSON file
loadAuditTrail :: FilePath -> IO (Maybe AuditTrail)
loadAuditTrail filePath = do
  content <- B.readFile filePath
  return $ decode content

-- Function to save the audit trail to a JSON file using a temporary file
saveAuditTrail :: FilePath -> AuditTrail -> IO ()
saveAuditTrail filePath auditTrail = do
  let tempFilePath = filePath <.> "tmp"
  withFile tempFilePath WriteMode $ \h -> do
    B.hPutStr h (encode auditTrail)
  renameFile tempFilePath filePath

-- Function to add a transaction to the audit trail
addTransaction :: [Transaction] -> Transaction -> [Transaction]
addTransaction txns txn = txn : txns

-- Function to generate a detailed report of all transactions
generateReport :: AuditTrail -> String
generateReport (AuditTrail txns _ _) =
  unlines $ "Detailed Report of Transactions:" : map showTransaction txns
  where
    showTransaction txn = unlines
      [ "Transaction ID: " ++ show (transactionId txn)
      , "Time: " ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (transactionTime txn)
      , "Amount: " ++ show (transactionAmount txn)
      , "Purpose: " ++ transactionPurpose txn
      , "Donor: " ++ T.unpack (transactionDonor txn)
      , "Allocations: " ++ show (transactionAllocations txn)
      ]

-- Function to generate analytics on donations and usage
generateAnalytics :: AuditTrail -> String
generateAnalytics (AuditTrail txns _ _) =
  unlines
    [ "Analytics:"
    , "Total Donations: " ++ show totalDonations
    , "Total Transactions: " ++ show totalTransactions
    , "Average Donation: " ++ show averageDonation
    , "Donations by Purpose: " ++ show donationsByPurpose
    , "Donations by Project: " ++ show donationsByProject
    ]
  where
    totalDonations = sum $ map transactionAmount txns
    totalTransactions = length txns
    averageDonation = if totalTransactions == 0 then 0 else totalDonations / fromIntegral totalTransactions
    donationsByPurpose = map (\grp -> (fst (head grp), sum (map snd grp))) groupedByPurpose
    groupedByPurpose = groupBy ((==) `on` fst) . sortOn fst $ map (\txn -> (transactionPurpose txn, transactionAmount txn)) txns
    donationsByProject = map (\grp -> (fst (head grp), sum (map snd grp))) groupedByProject
    groupedByProject = groupBy ((==) `on` fst) . sortOn fst $ concatMap (\txn -> map (\alloc -> (project alloc, amount alloc)) (transactionAllocations txn)) txns

-- Function to verify a project
verifyProject :: Project -> Project
verifyProject project = project { projectVerified = True }

-- Function to approve a project
approveProject :: Project -> Project
approveProject project = project { projectApproved = True }

-- Function to calculate the impact of donations
calculateImpact :: [Transaction] -> [Impact]
calculateImpact txns = concatMap assessImpact txns
  where
    assessImpact txn = map (createImpact txn) (transactionAllocations txn)
    createImpact txn alloc = Impact
      { impactId = transactionId txn
      , impactDescription = "Impact of donation to " ++ project alloc
      , impactValue = allocationImpactValue alloc
      , associatedProjectId = findProjectId (project alloc)
      }
    allocationImpactValue alloc = amount alloc * 1.5 -- Simplified calculation
    findProjectId projectName = 1 -- Simplified lookup

-- Function to generate a detailed impact report
generateImpactReport :: AuditTrail -> String
generateImpactReport (AuditTrail _ _ impacts) =
  unlines $ "Impact Report:" : map showImpact impacts
  where
    showImpact impact = unlines
      [ "Impact ID: " ++ show (impactId impact)
      , "Description: " ++ impactDescription impact
      , "Value: " ++ show (impactValue impact)
      , "Associated Project ID: " ++ show (associatedProjectId impact)
      ]

-- Create and submit a Cardano transaction
createAndSubmitTransaction :: Text -> Text -> Double -> IO ()
createAndSubmitTransaction senderAddr receiverAddr amount = do
  createTransaction senderAddr receiverAddr amount
  submitTransaction (makeTransaction Mainnet senderAddr receiverAddr amount)