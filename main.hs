import Data.Char  ( toLower, isPunctuation )
import Data.Maybe ( isNothing )
import System.IO  ( hFlush, stdout )

-- interactivity (aka IO or behavior)

main :: IO ()
main = do
    putStr "Give me a word: "
    hFlush stdout
    word <- getLine
    print $ verbose word

verbose :: String -> String
verbose word =
    case nonemptyPal word of
        Nothing    -> "Please enter a word."
        Just False -> "Sorry, this is not a palindrome."
        Just True  -> "Yay, it's a palindrome!"

-- definition (aka pure or logic)

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
