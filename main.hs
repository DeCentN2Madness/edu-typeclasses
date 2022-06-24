import Data.Char ( isAlpha )

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
    if all isAlpha string then Just string else Nothing

removeSpaces :: String -> Maybe String
removeSpaces string =
    case filter (\ x -> not (x == ' ')) string of
        ""     -> Nothing
        result -> Just result