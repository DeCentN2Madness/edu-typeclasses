import System.IO  ( hFlush, stdout )
import Pal        ( isPalindrome )

main :: IO ()
main = do
    putStr "Give me a word: "
    hFlush stdout
    getLine >>= (print . verbose)

verbose :: String -> String
verbose word =
    case isPalindrome word of
        Nothing    -> "Please enter a word."
        Just False -> "Sorry, this is not a palindrome."
        Just True  -> "Yay, it's a palindrome!"
