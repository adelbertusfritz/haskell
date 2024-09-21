check :: Eq a => [a] -> a -> Bool
check xs s = foldr(\x -> (||) (x==s)) False xs