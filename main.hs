import Data.Char  ( toLower, isPunctuation )
import Data.Maybe ( isNothing )
import System.IO  ( hFlush, stdout )

main :: IO ()
main = do
    putStr "Give me a word: "
    hFlush stdout
    word <- getLine
    print $ verbose word

isPalindrome :: String -> Bool
isPalindrome word =
    word == reverse word

isPalindromePhrase :: String -> Bool
isPalindromePhrase phrase =
    isPalindrome $ myFilter notSpace phrase

notSpace :: Char -> Bool
notSpace = (/= ' ')

notPunctuation :: Char -> Bool
notPunctuation char = not $ isPunctuation char

myFilter :: (Char -> Bool) -> String -> String
myFilter predicate string =
    case string of
        [] -> []
        first : remainder ->
            if predicate first
            then  first : myFilter predicate remainder
            else myFilter predicate remainder

withoutChar :: Char -> String -> String
withoutChar char phrase =
    case phrase of
        []                -> []
        char : remainder  -> withoutChar char remainder
        first : remainder -> first : withoutChar char remainder

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
    | a == Just True  = "W00T! Palindromitic effect in play, so let thee henceforth know the sons of Palindrome to be Palindromite!"
    | a == Just False = "Nice string, but it's not a palindrome."
    where a = nonemptyPal word

allLowerCase :: String -> String
allLowerCase = map toLower

myHead :: [a] -> a
myHead (first : rest) = first

myTail :: [a] -> [a]
myTail xs =
    case xs of
        []           -> []
        first : rest -> rest

isPalindromeIgnoringCase :: String -> Bool
isPalindromeIgnoringCase word =
    isPalindrome $ allLowerCase word
