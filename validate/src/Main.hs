module Main where

import Data.Char       ( isAlphaNum, isSpace )
import Data.Semigroup  ( (<>) )
import Data.Validation ( Validation(..) )

newtype Username = Username String            deriving (Eq, Show)
newtype Password = Password String            deriving (Eq, Show)
newtype Error    = Error [String]             deriving (Eq, Show)

data    User     = User Username Password     deriving (Eq, Show)

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
    Failure err -> putStrLn $ unlines $ errorCoerce err
    Success (User (Username name) pass) -> putStrLn $ "Welcome, " ++ name

makeUser :: Username -> Password -> Validation Error User
makeUser name pass =
  User <$> usernameErrors name <*> passwordErrors pass

errorCoerce :: Error -> [String]
errorCoerce (Error err) = err

passwordErrors :: Password -> Validation Error Password
passwordErrors pass =
  case validatePassword pass of
    Failure err -> Failure $ Error ["Invalid password: "] <> err
    Success pass -> Success pass

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

usernameErrors :: Username -> Validation Error Username
usernameErrors name =
  case validateUsername name of
    Failure err -> Failure $ Error ["Invalid username: "] <> err
    Success name -> Success name

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
