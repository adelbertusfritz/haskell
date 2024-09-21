import Text.Read

stringToListOfInts :: String -> [Int]
stringToListOfInts = map read . words
listOfIntsToString :: [Int] -> String
listOfIntsToString = unwords . map show
highAndLow :: String -> String
highAndLow n = listOfIntsToString [maximum (stringToListOfInts n), minimum (stringToListOfInts n)]

answer:: String -> String
answer xs = show (maximum ns) ++ " " ++ show (minimum ns)
    where ns = (map read $ words xs) :: [Int]

main:: IO()
main = do
    print (highAndLow "8 3 -5 42")