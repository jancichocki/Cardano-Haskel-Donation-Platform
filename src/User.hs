{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module User
  ( User(..)
  , registerUser
  , verifyUser
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import System.Random (randomRIO)

data User = User
  { userId :: Int
  , userName :: Text
  , userEmail :: Text
  , userVerified :: Bool
  } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

registerUser :: Text -> Text -> IO User
registerUser name email = do
  uid <- generateUserId
  let newUser = User uid name email False
  putStrLn $ "Registering user: " ++ T.unpack name
  return newUser

verifyUser :: User -> IO User
verifyUser user = do
  putStrLn $ "Performing KYC for user: " ++ T.unpack (userName user)
  let verifiedUser = user { userVerified = True }
  if userVerified verifiedUser
    then putStrLn $ "User " ++ T.unpack (userName user) ++ " has been verified."
    else putStrLn $ "User " ++ T.unpack (userName user) ++ " failed KYC."
  return verifiedUser

generateUserId :: IO Int
generateUserId = randomRIO (1000, 9999)



