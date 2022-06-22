import Data.Char  ( toLower )
import Data.Maybe ( isNothing )
import System.IO  ( hFlush, stdout )

isPalindrome :: String -> Bool
isPalindrome word =
    word == reverse word

nonemptyPal :: String -> Maybe Bool
nonemptyPal word =
    case word of
        "" -> Nothing
        _  -> Just $ isPalindrome word

verbose :: String -> String
verbose word =
    case nonemptyPal word of
        Nothing    -> "Please enter a word."
        Just False -> "Sorry, this is not a palindrome."
        Just True  -> "Yay, it's a palindrome!"

verbose' :: String -> String
verbose' word
    | isNothing a     = "We need some input."
    | a == Just True  = "W00T! Palindromitic effect in play!"
    | a == Just False = "Nice string, but it's not a palindrome."
    where a = nonemptyPal word

allLowerCase :: String -> String
allLowerCase = myMap toLower

myMap :: (a -> a) -> [a] -> [a]
myMap func list =
    case list of
        [] -> []
        (first : rest) -> func first : myMap func rest

myHead :: [a] -> a
myHead (first : rest) = first

myTail :: [a] -> [a]
myTail xs =
    case xs of
        []             -> []
        (first : rest) -> rest

isPalindromeIgnoringCase :: String -> Bool
isPalindromeIgnoringCase word =
    isPalindrome $ allLowerCase word

main :: IO ()
main = do
    putStr "Give me a word: "
    hFlush stdout
    word <- getLine
    print $ verbose word
