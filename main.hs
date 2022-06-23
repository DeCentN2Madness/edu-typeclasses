import Data.Char

rejectNonalphabetic :: String -> Maybe String
rejectNonalphabetic string =
    case myAll isAlpha string of
        False -> Nothing
        True  -> Just string

myAll :: (a -> Bool) -> [a] -> Bool
myAll pred = foldr (\ x y -> pred x && y) True

{- myAll _ [] = True
myAll pred (x:xs) =
    case pred x of
        False -> False
        True -> myAll pred xs -}

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny pred (x:xs) =
    case pred x of
        True -> True
        False -> myAny pred xs
