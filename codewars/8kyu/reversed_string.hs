module Solution where

solution :: String -> String
solution [] = []
solution (x:xs) = solution xs ++ [x]