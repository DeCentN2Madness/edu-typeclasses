import Data.Char

rejectNonalphabetic :: String -> Maybe String
rejectNonalphabetic string =
    case myAll myAlpha string of
        False -> Nothing
        True  -> Just string

myAll :: (a -> Bool) -> [a] -> Bool
myAll pred = foldr (\ x y -> pred x && y) True

myAlpha :: Char -> Bool
myAlpha c = elem c ['a'..'z'] || elem c ['A'..'Z']
