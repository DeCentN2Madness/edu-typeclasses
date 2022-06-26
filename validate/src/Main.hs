module Main where

import Data.Char (isAlphaNum, isSpace)

newtype Password =
  Password String deriving (Eq, Show)
newtype Error =
  Error String deriving (Eq, Show)
newtype Username =
  Username String deriving (Eq, Show)

data User = User Username Password
  deriving (Eq, Show)

main :: IO ()
main = do
  putStr "Please enter a password: "
  password <- getLine
  print (validatePassword password)

validatePassword :: Password -> Either Error Password
validatePassword (Password pass) =
  stripSpace pass >>= allAlpha >>= passwordLength

passwordLength :: String -> Either Error Password
passwordLength "" = Left $ Error "empty password not allowed"
passwordLength xs =
  if length xs > 20
    then Left $ Error "password must be less than 20 characters"
    else Right $ Password xs

usernameLength :: String -> Either Error Username
usernameLength "" = Left $ Error "empty username not allowed"
usernameLength xs =
  if length xs > 15
    then Left $ Error "username must be less than 15 characters"
    else Right $ Username xs

allAlpha :: String -> Either Error String
allAlpha "" = Left $ Error "empty password not allowed"
allAlpha xs =
  if all isAlphaNum xs
    then Right xs
    else Left $ Error "white space and special characters not allowed"

stripSpace :: String -> Either Error String
stripSpace "" = Left $ Error "empty password not allowed"
stripSpace (x:xs) =
  if isSpace x
    then stripSpace xs
    else Right (x:xs)

{-------------------------
-- here be deprecated code

checkPassword :: String -> Maybe String
checkPassword xs =
  case stripSpace xs of
    Nothing -> Nothing
    Just xs' ->
      case allAlpha xs' of
        Nothing -> Nothing
        Just xs' ->
          case maxLength xs' of
            Nothing -> Nothing
            Just xs' -> Just xs'

checkPasswd :: String -> String
checkPasswd pass =
  case stripSpace pass of
    Nothing    -> "empty password not allowed"
    Just pass' ->
      case allAlpha pass' of
        Nothing    -> "white space and special characters not allowed"
        Just pass' ->
          case maxLength pass' of
            Nothing    -> "password too long"
            Just pass' -> pass'
-}
