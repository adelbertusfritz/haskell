acc:: String -> (String, Int) -> Int -> Int
acc b t i
    | fst t == b = 2 * snd t + i
    | otherwise = snd t + i

outed :: [(String,Int)] -> String -> String
outed l b 
    | fromIntegral (foldr (acc b) 0 l) / fromIntegral (length l) <= 5 = "Get Out Now!"
    | otherwise = "Nice Work Champ!"
