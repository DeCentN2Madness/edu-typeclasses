import Data.Char

rejectNonalphabetic :: String -> Maybe String
rejectNonalphabetic string =
    case myAll isAlpha string of
        False -> Nothing
        True  -> Just string

myAll :: (a -> Bool) -> [a] -> Bool
myAll pred = foldr (\ x y -> pred x && y) True
