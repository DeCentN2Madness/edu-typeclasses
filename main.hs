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
    | a == Nothing    = "We need some input."
    | a == Just True  = "W00T! Palindromitic effect in play!"
    | a == Just False = "Nice string, but it's not a palindrome."
    where a = nonemptyPal word

main :: IO ()
main = do
    word <- getLine
    print $ verbose word
