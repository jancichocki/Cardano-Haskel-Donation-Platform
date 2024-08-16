{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE OverloadedStrings #-}

import AuditTrail
import CardanoTransactions
import Data.Time.Clock (getCurrentTime)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import System.IO (withFile, IOMode(..), hClose)
import Control.Exception (bracket)
import System.Directory (renameFile)
import System.FilePath ((<.>))

main :: IO ()
main = do
  putStrLn "1. Register and Donate"
  putStrLn "2. Verify and Approve Projects"
  putStrLn "3. Measure and Report Impact"
  putStrLn "4. Create and Submit Cardano Transaction"
  putStrLn "Enter your choice:"
  choice <- readLn :: IO Int
  case choice of
    1 -> registerAndDonate
    2 -> verifyAndApproveProjects
    3 -> measureAndReportImpact
    4 -> createAndSubmitCardanoTransaction
    _ -> putStrLn "Invalid choice"

registerAndDonate :: IO ()
registerAndDonate = do
  putStrLn "Enter your name:"
  name <- T.pack <$> getLine
  putStrLn "Enter your email:"
  email <- T.pack <$> getLine

  let verifiedUser = User { userId = 1545, userName = name, userEmail = email, userVerified = True }

  putStrLn $ "Registering user: " ++ T.unpack name
  putStrLn $ "Performing KYC for user: " ++ T.unpack name
  putStrLn $ "User " ++ T.unpack name ++ " has been verified."

  putStrLn "Enter donation amount:"
  amount <- readLn :: IO Double

  putStrLn "Enter the number of allocations:"
  numAllocations <- readLn :: IO Int
  allocations <- mapM getAllocation [1..numAllocations]

  putStrLn "Select a donation category:"
  putStrLn "1. Education"
  putStrLn "2. Healthcare"
  putStrLn "3. Environment"
  putStrLn "4. Animal Welfare"
  putStrLn "5. Community Development"
  categoryChoice <- readLn :: IO Int
  let category = case categoryChoice of
                    1 -> "Education"
                    2 -> "Healthcare"
                    3 -> "Environment"
                    4 -> "Animal Welfare"
                    5 -> "Community Development"
                    _ -> "Other"

  currentTime <- getCurrentTime
  let transaction = Transaction
        { transactionId = 1
        , transactionTime = currentTime
        , transactionAmount = amount
        , transactionPurpose = T.unpack category
        , transactionDonor = userName verifiedUser
        , transactionAllocations = allocations
        }

  -- Load the current audit trail
  maybeAuditTrail <- loadAuditTrail "auditTrail.json"
  let auditTrail = case maybeAuditTrail of
                     Nothing -> AuditTrail [] [] []
                     Just at -> at

  -- Add the new transaction to the audit trail
  let newTransactions = addTransaction (transactions auditTrail) transaction
      newAuditTrail = auditTrail { transactions = newTransactions }

  -- Save the updated audit trail
  saveAuditTrail "auditTrail.json" newAuditTrail

  -- Generate and print the report
  putStrLn $ generateReport newAuditTrail

  -- Generate and print the analytics
  putStrLn $ generateAnalytics newAuditTrail

verifyAndApproveProjects :: IO ()
verifyAndApproveProjects = do
  putStrLn "Enter the project ID to verify:"
  projectId <- readLn :: IO Int
  putStrLn "Enter the project name:"
  projectName <- getLine
  putStrLn "Enter the project description:"
  projectDescription <- getLine

  let project = Project
        { projectId = projectId
        , projectName = projectName
        , projectDescription = projectDescription
        , projectVerified = False
        , projectApproved = False
        }

  putStrLn "Verifying project..."
  let verifiedProject = verifyProject project
  putStrLn $ "Project " ++ projectName ++ " has been verified."

  putStrLn "Approving project..."
  let approvedProject = approveProject verifiedProject
  putStrLn $ "Project " ++ projectName ++ " has been approved."

  -- Load the current audit trail
  maybeAuditTrail <- loadAuditTrail "auditTrail.json"
  let auditTrail = case maybeAuditTrail of
                     Nothing -> AuditTrail [] [] []
                     Just at -> at

  -- Add the new project to the audit trail
  let newProjects = approvedProject : projects auditTrail
      newAuditTrail = auditTrail { projects = newProjects }

  -- Save the updated audit trail
  saveAuditTrail "auditTrail.json" newAuditTrail

  -- Generate and print the report
  putStrLn "Projects Report:"
  mapM_ (putStrLn . show) newProjects

measureAndReportImpact :: IO ()
measureAndReportImpact = do
  -- Load the current audit trail
  maybeAuditTrail <- loadAuditTrail "auditTrail.json"
  let auditTrail = case maybeAuditTrail of
                     Nothing -> AuditTrail [] [] []
                     Just at -> at

  -- Calculate the impact
  let impacts = calculateImpact (transactions auditTrail)
      newAuditTrail = auditTrail { impacts = impacts }

  -- Save the updated audit trail
  saveAuditTrail "auditTrail.json" newAuditTrail

  -- Generate and print the impact report
  putStrLn $ generateImpactReport newAuditTrail

createAndSubmitCardanoTransaction :: IO ()
createAndSubmitCardanoTransaction = do
  putStrLn "Enter the sender address:"
  senderAddr <- T.pack <$> getLine
  putStrLn "Enter the receiver address:"
  receiverAddr <- T.pack <$> getLine
  putStrLn "Enter the amount to send:"
  amount <- readLn :: IO Double

  createAndSubmitTransaction senderAddr receiverAddr amount

getAllocation :: Int -> IO Allocation
getAllocation n = do
  putStrLn $ "Enter the project name for allocation #" ++ show n ++ ":"
  projectName <- getLine
  putStrLn $ "Enter the amount for allocation #" ++ show n ++ ":"
  allocationAmount <- readLn :: IO Double
  return $ Allocation projectName allocationAmount





