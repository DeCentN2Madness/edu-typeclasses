module Main where

import Data.Char (isAlphaNum)

main :: IO ()
main = do
  putStrLn "Please enter a password"
  password <- getLine
  print (allAlpha password)

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
