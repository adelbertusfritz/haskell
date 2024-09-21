import qualified Data.Set as Set
import Data.Char (chr, ord, isAlpha, isLower, isUpper, toLower, isAlpha)


main :: IO ()
main = do
    print generateCard
    print (wellSuite(Hand (Card Spade Ace) (Card Spade Ace)))
    print (power(Hand Joker (Card Spade Ace)))
    print (strongerHand(Hand (Card Spade Two) (Card Spade Two))(Hand (Card Diamond Eight) (Card Diamond Eight)))
    let hands = [Hand (Card Spade Two) (Card Spade Two), Hand (Card Spade Three) (Card Spade Three)]
    let hands2 = [Hand (Card Spade Two) (Card Spade Three), Hand (Card Spade Four) (Card Spade Five)]
    
    print (sortHands hands)
    print (handsToCards hands)
    print (checkValidity hands)
    print (checkValidity hands2)

    print (caesarCipher "Adel" 3 False)
    print (countChar "Fritz Adelbertus Sitindaon")
    print (primes 10)
    print (primeProduct [60,150])

-- Soal 1
data Suite = Spade | Heart | Club | Diamond deriving (Eq,Show,Ord) 
data CardValue = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Ord)
data Card = Joker | Card { suite :: Suite, value:: CardValue} deriving (Eq, Ord)

instance Show CardValue where
    show :: CardValue -> String
    show Ace    = "Ace"
    show Two    = "2"
    show Three  = "3"
    show Four   = "4"
    show Five   = "5"
    show Six    = "6"
    show Seven  = "7"
    show Eight  = "8"
    show Nine   = "9"
    show Ten    = "10"
    show Jack   = "Jack"
    show Queen  = "Queen"
    show King   = "King"

instance Show Card where
    show :: Card -> String
    show (Card s v) = "(" ++ show s ++ ", " ++ show v ++ ")"
    show Joker = "Joker"

cardValues:: [CardValue]
cardValues = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]

generateCard:: [Card]
generateCard = [Card {suite=x, value=y} | x <- [Spade, Heart, Club, Diamond], y <- cardValues] ++ [Joker]



-- Soal 2
data Hand = Hand Card Card deriving (Eq, Ord)

instance Show Hand where
    show :: Hand -> String
    show (Hand (Card a b) (Card c d)) = "(" ++ show a ++ " " ++ show b ++ "," ++ show c ++ " " ++ show d ++ ")"
    show (Hand Joker (Card c d)) = "(Joker," ++ show c ++ " " ++ show d ++ ")"
    show (Hand (Card a b) Joker) = "(" ++ show a ++ " " ++ show b ++ ",Joker)"

wellSuite:: Hand -> Bool
wellSuite (Hand (Card a b) (Card c d)) = a == c

pair:: Hand -> Bool
pair (Hand (Card a b) (Card c d)) = b == d

haveJoker:: Hand -> Bool
haveJoker (Hand a b) = a == Joker || b == Joker

power:: (Num a, Ord a) => Hand -> a
power h
    | haveJoker h = 5
    | pair h && wellSuite h = 5
    | pair h = 3
    | wellSuite h = 2
    | otherwise = 1

strongestCardInHand:: Hand -> Card
strongestCardInHand (Hand (Card a b) (Card c d))
    | b > d = Card a b
    | otherwise = Card c d

getCardValue:: Card -> CardValue
getCardValue (Card a b) = b

compareCardValue:: Hand -> Hand -> Bool
compareCardValue a b = getCardValue (strongestCardInHand a) > getCardValue (strongestCardInHand b)

strongerHand :: Hand -> Hand -> Bool
strongerHand a b
    | a == b = False
    | power a > power b = True
    | power a < power b = False
    | otherwise = compareCardValue a b


insert :: Ord a => a -> (a -> a -> Bool) -> [a] -> [a]
insert x f [] = [x]
insert x f (y:ys) 
    | f x y = x:y:ys
    | otherwise = y : insert x f ys

insertionSort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
insertionSort f [x] = [x]
insertionSort f (x:xs) = insert x f (insertionSort f xs)

sortHands:: [Hand] -> [Hand]
sortHands = insertionSort strongerHand

-- Soal 3
getCardsfromHand:: Hand -> [Card]
getCardsfromHand (Hand a b) = [a, b]
handsToCards:: [Hand] -> [Card]
handsToCards a = [x | y <- a, x <- getCardsfromHand y]


checkValidity:: [Hand] -> Bool
checkValidity a = checkValidity' Set.empty (handsToCards a)
    where
        checkValidity' _ [] = True
        checkValidity' seen (x:xs)
            | x `Set.member` seen = False
            | otherwise = checkValidity' (Set.insert x seen) xs


-- Soal 4

shift :: Integral a => (Int -> Int -> Int) -> Char -> Char -> a -> Char
shift f c i n = chr $ (f (ord c - ord i) (fromIntegral n) `mod` 26) + ord i

toLeft :: Int -> Int -> Int
toLeft c n = c - n
toRight :: Int -> Int -> Int
toRight c n = c + n

shiftChar :: Integral a => a -> Bool -> Char -> Char
shiftChar key mode text
    | isLower text && mode = shift toRight text 'a' key
    | isLower text && not mode = shift toLeft text 'a' key
    | isUpper text && mode = shift toRight text 'A' key
    | isUpper text && not mode = shift toLeft text 'A' key
    | otherwise = text

caesarCipher :: Integral a => [Char] -> a -> Bool -> [Char]
caesarCipher text key mode = map (shiftChar key mode) text

-- Soal 5
charAlreadyIn :: Char -> [(Char,Integer)] -> Bool
charAlreadyIn a [] = False
charAlreadyIn a (x:xs)
    | fst x == a = True
    | otherwise = charAlreadyIn a xs

updateTupleList :: Char -> [(Char,Integer)] -> [(Char,Integer)]
updateTupleList a (x:xs)
    | fst x == a = (fst x, snd x + 1) : xs
    | otherwise = x : updateTupleList a xs

updateChar :: Char -> [(Char,Integer)] -> [(Char, Integer)]
updateChar a b
    | charAlreadyIn a b = updateTupleList a b 
    | otherwise = (a,1) : b

countChar :: [Char] -> [(Char, Integer)]
countChar s = foldr (updateChar . toLower) [] (filter isAlpha s)


-- Soal 6
isPrime :: Integral a => a -> Bool
isPrime n = length [x | x <- [1..n], n `mod` x == 0] == 2

primes :: Integral a => a -> [a]
primes n = [x | x <-[1..n], isPrime x]

primeDivider :: Integral a => a -> [(a,a)]
primeDivider n = [(x,0) | x <- primes n, n `mod` x == 0]

primeFactorize:: Integral a => a -> [(a,a)]
primeFactorize n = factorize' n (primeDivider n) 
    where factorize' 1 l = l
          factorize' m (x:xs) 
            | m `mod` fst x == 0 = factorize' (m `div` fst x) ((fst x, snd x+1):xs)
            | otherwise = x : factorize' m xs

primeProduct :: Integral a => [a] -> [[(a,a)]]
primeProduct = map primeFactorize

