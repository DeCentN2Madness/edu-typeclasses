{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Char       ( isAlphaNum, isSpace )
import Data.Coerce     ( coerce )
import Data.Semigroup  ( (<>) )
import Data.Validation ( Validation(..) )

newtype Username = Username String            deriving (Eq, Show)
newtype Password = Password String            deriving (Eq, Show)
newtype Error    = Error [String]             deriving (Eq, Show)

data    User     = User Username Password     deriving (Eq, Show)

type    Rule a   = a -> Validation Error a

instance Semigroup Error where
  Error xs <> Error ys = Error $ xs <> ys

main :: IO ()
main = do
  putStr "Please enter a username: "
  username <- Username <$> getLine
  putStr "Please enter a password: "
  password <- Password <$> getLine
  display username password

display :: Username -> Password -> IO ()
display name pass =
  case makeUser name pass of
    Failure err -> putStrLn $ unlines $ coerce err
    Success (User name pass) ->
      putStrLn $ "Welcome, " ++ coerce @Username @String name

makeUser :: Username -> Password -> Validation Error User
makeUser name pass =
  User <$> usernameErrors name <*> passwordErrors pass

passwordErrors :: Rule Password
passwordErrors pass =
  case validatePassword pass of
    Failure err -> Failure $ Error ["Invalid password: "] <> err
    Success pass -> Success pass

validatePassword :: Rule Password
validatePassword pass =
  case (coerce stripSpace :: Rule Password) pass of
    Failure err   -> Failure err
    Success pass' ->
      (coerce allAlpha :: Rule Password) pass' *>
      (coerce passwordLength :: Rule Password) pass'

passwordLength :: String -> Validation Error Password
passwordLength "" = Failure $ Error ["empty password not allowed"]
passwordLength xs =
  if length xs > 20
    then Failure $ Error ["password must be less than 20 characters"]
    else Success $ Password xs

usernameErrors :: Rule Username
usernameErrors name =
  case validateUsername name of
    Failure err -> Failure $ Error ["Invalid username: "] <> err
    Success name -> Success name

validateUsername :: Rule Username
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

allAlpha :: Rule String
allAlpha "" = Failure $ Error ["empty input not allowed"]
allAlpha xs =
  if all isAlphaNum xs
    then Success xs
    else Failure $ Error ["white space and special characters not allowed"]

stripSpace :: Rule String
stripSpace "" = Failure $ Error ["empty input not allowed"]
stripSpace (x:xs) =
  if isSpace x
    then stripSpace xs
    else Success (x:xs)
