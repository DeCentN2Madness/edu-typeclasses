module Pal where

import Data.Char  ( toLower, isPunctuation )

isPalindrome :: String -> Maybe Bool
isPalindrome = undefined

isOwnReverse :: String -> Bool
isOwnReverse word =
    word == reverse word

isPalindromePhrase :: String -> Bool
isPalindromePhrase phrase =
    isOwnReverse $ filter notSpace phrase

notSpace :: Char -> Bool
notSpace = (/= ' ')

notPunctuation :: Char -> Bool
notPunctuation char = not $ isPunctuation char

nonemptyPal :: String -> Maybe Bool
nonemptyPal word =
    case word of
        "" -> Nothing
        _  -> Just $ isOwnReverse word

allLowerCase :: String -> String
allLowerCase = map toLower

isPalindromeIgnoringCase :: String -> Bool
isPalindromeIgnoringCase word =
    isOwnReverse $ allLowerCase word
