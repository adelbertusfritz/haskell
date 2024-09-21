import Data.Char (chr, ord, isAlpha, isLower, isUpper, toLower)

-- Soal 1

--------------------- Dari Latihan 3 ------------------
-- Terapkan partial application
shift :: Integral a => (Int -> Int -> Int) -> Char -> Char -> a -> Char
shift f c i n = chr $ (f (ord c - ord i) (fromIntegral n) `mod` 26) + ord i

shiftLeft :: Integral a => Char -> Char -> a -> Char
shiftLeft = shift (-)

shiftRight :: Integral a => Char -> Char -> a -> Char
shiftRight = shift (+)

shiftChar :: Integral a => a -> Bool -> Char -> Char
shiftChar key mode text
    | isLower text && mode = shiftRight text 'a' key
    | isLower text && not mode = shiftLeft text 'a' key
    | isUpper text && mode = shiftRight text 'A' key
    | isUpper text && not mode = shiftLeft text 'A' key
    | otherwise = text
----------------------------------------------------------

charToShift:: Char -> Int
charToShift c = (ord c - 13) `mod` 26

vigenere :: [Char] -> [Char] -> Bool -> [Char]
vigenere text key mode = zipWith (curry applyShift) text $ indexOf text
    where 
        indexOf :: [Char] -> [Int]
        indexOf n = fst $ foldl numerate ([],0) n
        numerate :: ([Int],Int) -> Char -> ([Int], Int)
        numerate (l,i) e
            | isAlpha e = (l ++ [i] , i+1)
            | otherwise = (l ++ [-1], i)
        shifts :: [Int]
        shifts = map charToShift key
        applyShift :: (Char, Int) -> Char
        applyShift (a,b) = shiftChar (shifts !! mod b (length key)) mode a 


-- Soal 2

elemInTarget :: Eq a => a -> [a] -> Bool
elemInTarget b = foldl (\x y -> x || (b == y)) False

reducer :: (Eq a ) => (a -> [a]) -> [a] -> [a] -> a -> [a]
reducer f b x y = if elemInTarget y b then x ++ f y else x ++ [y]

occurence :: (Foldable t, Eq a) => (a -> [a]) -> [a] -> t a -> [a]
occurence f b = foldl (reducer f b) []

-- Apply point-free style :D
repeatOccurence :: (Integral a, Eq b) => a -> [b] -> [b] -> [b]
repeatOccurence = occurence . replicate . fromIntegral

-- Soal 3

manhattan :: Num a => (a, a) -> (a, a) -> a
manhattan (a,b) (c,d) = abs(a-c) + abs(b-d) 

calculateDistances :: Num a => [(a, a)] -> [[a]]
-- Partially applicate manhattan
calculateDistances l = map (\x -> map (manhattan x) l) l

-- Soal 4
remove :: a -> [a]
remove = const []
removeOccurence :: Eq b => [b] -> [b] -> [b]
-- Partially applicate occurence
removeOccurence = occurence remove

main:: IO()
main = do
    print (vigenere "K ip KTUQVQ" "CIDKAGENOU" False)
    print (repeatOccurence 3 "love" "hello")
    print (removeOccurence "love" "hello")
    print (manhattan (1,1) (2,3))
    print (calculateDistances [(1,1), (2,2), (3,3), (4,5)])