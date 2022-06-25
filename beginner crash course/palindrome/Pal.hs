module Pal where

import Data.Char  ( toLower, isPunctuation )

isPalindrome :: String -> Maybe Bool
isPalindrome = isOwnReverseMaybe . rejectEmpty . normalize

normalize :: String -> String
normalize = filter notSpace . allLowerCase . filter notPunctuation

notSpace :: Char -> Bool
notSpace = (/= ' ')

allLowerCase :: String -> String
allLowerCase = map toLower

notPunctuation :: Char -> Bool
notPunctuation char = not $ isPunctuation char

rejectEmpty :: String -> Maybe String
rejectEmpty word =
    case word of
        "" -> Nothing
        _  -> Just word

isOwnReverseMaybe :: Maybe String -> Maybe Bool
isOwnReverseMaybe maybeString =
    case maybeString of
        Nothing -> Nothing
        Just string -> Just $ isOwnReverse string

isOwnReverse :: String -> Bool
isOwnReverse word =
    word == reverse word
