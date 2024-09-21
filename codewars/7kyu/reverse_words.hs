import Data.List (groupBy)
import Data.Char (isSpace)

separate :: String -> [String]
separate = groupBy (\a b -> a == ' ' && b == ' ')

reverseWords :: String -> String
reverseWords n = concatMap reverse $ separate n
        

main :: IO()
main = do
    print (reverseWords "\na")
    print (concat $ map reverse $ separate "\n a")