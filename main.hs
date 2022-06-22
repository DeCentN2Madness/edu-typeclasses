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
verbose word
    | isNothing a     = "We need some input."
    | a == Just True  = "W00T! Palindromitic effect in play!"
    | a == Just False = "Nice string, but it's not a palindrome."
    where a = nonemptyPal word

main :: IO ()
main = do
    putStr "Give me a word: "
    hFlush stdout
    word <- getLine
    print $ verbose word
