module Main where

main :: IO ()
main = do
  putStrLn "Please enter a password"
  password <- getLine
  print (maxLength password)

maxLength :: String -> Maybe String
maxLength "" = Nothing
maxLength xs =
  case (length xs > 20) of
    True -> Nothing
    False -> Just xs
