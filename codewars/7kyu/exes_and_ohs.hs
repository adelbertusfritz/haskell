import Data.Char (toLower)

xo :: String -> Bool
xo str = judge $ foldl reduce (0,0) str
    where
        reduce (a,b) c
            | toLower c == 'x' = (a+1,b)
            | toLower c == 'o' = (a,b+1)
            | otherwise = (a,b)
        judge (a,b)
            | a == b = True
            | otherwise = False