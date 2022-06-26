module Main where

import Data.Char (isAlphaNum, isSpace)

main :: IO ()
main = do
  putStr "Please enter a password: "
  password <- getLine
  print (validatePassword password)

validatePassword :: String -> Maybe String
validatePassword pass = stripSpace pass >>= maxLength >>= allAlpha

maxLength :: String -> Maybe String
maxLength "" = Nothing
maxLength xs =
  if length xs > 20 then Nothing else Just xs

allAlpha :: String -> Maybe String
allAlpha "" = Nothing
allAlpha xs =
  if all isAlphaNum xs then Just xs else Nothing

stripSpace :: String -> Maybe String
stripSpace "" = Nothing
stripSpace (x:xs) =
  if isSpace x then stripSpace xs else Just (x:xs)

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
