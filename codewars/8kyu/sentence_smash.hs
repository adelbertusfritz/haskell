
smash:: [String] -> String
smash [] = ""
smash xs = foldl1(\x y -> x ++ " " ++ y) xs

main:: IO()
main = do
    print (smash ["hello", "world"])