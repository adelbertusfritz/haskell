
data DartScore
    = MissDS
    | SingleDS Int
    | DoubleDS Int
    | TripleDS Int
    | SingleBullDS
    | DoubleBullDS
    deriving (Eq, Show)


shotRadius:: Double -> Double -> Double
shotRadius a b = sqrt (a*a+b*b)
shotTheta:: Double -> Double -> Double
shotTheta a b
    | a < 0 && b < 0 = theta a b + 10.5
    | a < 0 && b > 0 = theta a b + 10.5
    | a > 0 && b < 0 = theta a b + 20.5
    | otherwise = theta a b + 0.5
    where theta a b = (atan (b / a) / pi) * 10
score:: [Int]
score = [6, 13, 4, 18, 1, 20, 5, 12, 9, 14, 11, 8, 16, 7, 19, 3, 17, 2, 15, 10]
digitScore:: Double -> Int
digitScore a = score !! floor a

getDartScore :: Double -> Double -> DartScore
getDartScore a b
    | shotRadius a b > 170 = MissDS
    | shotRadius a b < 6.35 = DoubleBullDS
    | shotRadius a b < 15.9 = SingleBullDS
    | shotRadius a b < 107 && shotRadius a b > 99 = TripleDS $ normalScore a b
    | shotRadius a b < 170 && shotRadius a b > 162 = DoubleDS $ normalScore a b
    | otherwise = SingleDS $ normalScore a b
    where normalScore a b = digitScore $ shotTheta a b

main :: IO()
main = do
    print (getDartScore (73.905) (-95.94))
    print (floor (shotTheta (73.905) (-95.94) + 1) * (-1))
    