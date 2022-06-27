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

instance Semigroup Error where
  Error xs <> Error ys = Error $ xs <> ys

data User = User Username Password
  deriving (Eq, Show)

main :: IO ()
main = do
  putStr "Please enter a username: "
  username <- Username <$> getLine
  putStr "Please enter a password: "
  password <- Password <$> getLine
  print $ makeUser username password

makeUser :: Username -> Password -> Validation Error User
makeUser name pass =
  User <$> validateUsername name <*> validatePassword pass

validatePassword :: Password -> Validation Error Password
validatePassword (Password pass) =
  case stripSpace pass of
    Failure err   -> Failure err
    Success pass' ->
      allAlpha pass' *> passwordLength pass'

passwordLength :: String -> Validation Error Password
passwordLength "" = Failure $ Error ["empty password not allowed"]
passwordLength xs =
  if length xs > 20
    then Failure $ Error ["password must be less than 20 characters"]
    else Success $ Password xs

validateUsername :: Username -> Validation Error Username
validateUsername (Username name) =
  case stripSpace name of
    Failure err   -> Failure err
    Success name' ->
      allAlpha name' *> usernameLength name'

usernameLength :: String -> Validation Error Username
usernameLength "" = Failure $ Error ["empty username not allowed"]
usernameLength xs =
  if length xs > 15
    then Failure $ Error ["username must be less than 15 characters"]
    else Success $ Username xs

allAlpha :: String -> Validation Error String
allAlpha "" = Failure $ Error ["empty input not allowed"]
allAlpha xs =
  if all isAlphaNum xs
    then Success xs
    else Failure $ Error ["white space and special characters not allowed"]

stripSpace :: String -> Validation Error String
stripSpace "" = Failure $ Error ["empty input not allowed"]
stripSpace (x:xs) =
  if isSpace x
    then stripSpace xs
    else Success (x:xs)
