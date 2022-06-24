import Data.Char ()

database :: [(Integer, String)]
database = [(1, "Julie"),
            (2, "Chris"),
            (3, "Alonzo"),
            (4, "Melman")]

greetUser :: Integer -> Maybe String
greetUser record =
    fmap ("Hello, " ++) (lookup record database)

rejectNonalphabetic :: String -> Maybe String
rejectNonalphabetic string =
    case myAll myAlpha string of
        False -> Nothing
        True  -> Just string

myAll :: (a -> Bool) -> [a] -> Bool
myAll pred = foldr (\ x y -> pred x && y) True

myAlpha :: Char -> Bool
myAlpha c = elem c ['A'..'z'] && notElem c "[\\]^_`"
