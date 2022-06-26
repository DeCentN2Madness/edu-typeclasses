module Main where

import Data.Char (isAlphaNum, isSpace)

main :: IO ()
main = do
  putStr "Please enter a password: "
  password <- getLine
  print (checkPassword password)

maxLength :: String -> Maybe String
maxLength "" = Nothing
maxLength xs =
  case (length xs > 20) of
    True -> Nothing
    False -> Just xs

allAlpha :: String -> Maybe String
allAlpha "" = Nothing
allAlpha xs =
  case all isAlphaNum xs of
    False -> Nothing
    True  -> Just xs

stripSpace :: String -> Maybe String
stripSpace "" = Nothing
stripSpace (x:xs) =
  case isSpace x of
    True  -> stripSpace xs
    False -> Just (x:xs)

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
