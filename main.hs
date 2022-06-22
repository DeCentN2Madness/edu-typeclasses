isPalindrome :: String -> Bool
isPalindrome word =
    word == reverse word

nonemptyPal :: String -> Maybe Bool

main :: IO ()
main = do
    word <- getLine
    print $ nonemptyPal word
