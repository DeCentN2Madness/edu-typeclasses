isPalindrome :: String -> Bool
isPalindrome word =
    word == reverse word

nonemptyPal :: String -> Maybe Bool
nonemptyPal word =
    case word of
        "" -> Nothing
        _  -> Just $ isPalindrome word

verbose :: String -> String

main :: IO ()
main = do
    word <- getLine
    print $ verbose word
