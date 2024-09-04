-- String ends with?

main :: IO ()
main = do
    print(solution "abcde" "de")

solution :: String -> String -> Bool
solution a b = endsWith b (extractEndString a b)

extractEndString :: String -> String -> String
extractEndString a b = drop (length a - length b) a

endsWith :: String -> String -> Bool
endsWith (a:aa) (b:bb) = a == b && endsWith aa bb
endsWith [] [] = True
endsWith _ _ = False

