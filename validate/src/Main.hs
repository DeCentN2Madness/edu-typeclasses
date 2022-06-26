module Main where

import Data.Char (isAlphaNum, isSpace)

main :: IO ()
main = do
  putStr "Please enter a password: "
  password <- getLine
  print (validatePassword password)

validatePassword :: String -> Either String String
validatePassword pass = stripSpace pass >>= maxLength >>= allAlpha

maxLength :: String -> Either String String
maxLength "" = Left "empty password not allowed"
maxLength xs =
  if length xs > 20
    then Left "must be less than 20 characters"
    else Right xs

allAlpha :: String -> Either String String
allAlpha "" = Left "empty password not allowed"
allAlpha xs =
  if all isAlphaNum xs
    then Right xs
    else Left "white space and special characters not allowed"

stripSpace :: String -> Either String String
stripSpace "" = Left "empty password not allowed"
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
