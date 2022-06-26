module Main where

import Data.Char (isAlphaNum, isSpace)
import Data.Semigroup
import Data.Validation

newtype Password =
  Password String deriving (Eq, Show)
newtype Error =
  Error [String] deriving (Eq, Show)
newtype Username =
  Username String deriving (Eq, Show)

data User = User Username Password
  deriving (Eq, Show)

main :: IO ()
main = do
  putStr "Please enter a username: "
  username <- Username <$> getLine
  putStr "Please enter a password: "
  password <- Password <$> getLine
  print $ validateUsername username
  print $ validatePassword password

validatePassword :: Password -> Either Error Password
validatePassword (Password pass) =
  stripSpace pass >>= allAlpha >>= passwordLength

passwordLength :: String -> Either Error Password
passwordLength "" = Left $ Error ["empty password not allowed"]
passwordLength xs =
  if length xs > 20
    then Left $ Error ["password must be less than 20 characters"]
    else Right $ Password xs

validateUsername :: Username -> Either Error Username
validateUsername (Username name) =
  stripSpace name >>= allAlpha >>= usernameLength

usernameLength :: String -> Either Error Username
usernameLength "" = Left $ Error ["empty username not allowed"]
usernameLength xs =
  if length xs > 15
    then Left $ Error ["username must be less than 15 characters"]
    else Right $ Username xs

allAlpha :: String -> Either Error String
allAlpha "" = Left $ Error ["empty password not allowed"]
allAlpha xs =
  if all isAlphaNum xs
    then Right xs
    else Left $ Error ["white space and special characters not allowed"]

stripSpace :: String -> Either Error String
stripSpace "" = Left $ Error ["empty password not allowed"]
stripSpace (x:xs) =
  if isSpace x
    then stripSpace xs
    else Right (x:xs)
