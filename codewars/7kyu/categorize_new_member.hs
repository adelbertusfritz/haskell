
data Membership = Open | Senior

openOrSenior:: [(Int, Int)] -> [Membership]
openOrSenior = map judge 
    where 
        judge :: (Int, Int) -> Membership
        judge (a,b)
            | a >= 55 && b > 7 = Senior
            | otherwise = Open
