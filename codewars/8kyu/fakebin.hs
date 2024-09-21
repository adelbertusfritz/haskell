import Data.Char
fakeBin:: String -> String
fakeBin [] = []
fakeBin (x:xs) = convertDigit x : fakeBin xs
    where 
        convertDigit:: Char -> Char 
        convertDigit a 
            | digitToInt a < 5 = '0'
            | otherwise = '1'


fakeBin2 :: String -> String
fakeBin2 = map (\c -> if c < '5' then '0' else '1' )
