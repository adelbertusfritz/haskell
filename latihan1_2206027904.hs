
main :: IO ()
main = do
    print(findPerimeter 1 2 3 4)
    print(isRectangle 1 4 1 4)
    print(listSum [1, 2, 3, 4])
    print(listSumPerimeter [(1,2,3,4), (1,2,3,4)])
    print(reverseList [1,2,3,4])
    print(insertionSortMaxSideAsc [(1,1,1,5), (2,3,4,3), (1,2,2,2)])
    print(insertionSortPerimeterDesc [(1,1,1,5), (2,3,4,3), (1,2,2,2)])
    print(filterEven [1,2,3,4])
    print(filterRectangle [(1,2,3,4), (1,1,1,1)]) 
    print(filterNotRectangle [(1,2,3,4), (1,1,1,1)])
 
-- 1 findPerimeter

findPerimeter :: Num a => a -> a -> a -> a -> a
findPerimeter a b c d = a + b + c + d

-- 2 isRectangle

isRectangle :: Eq a => a -> a -> a -> a -> Bool
isRectangle a b c d = a == c && b == d

-- 3 listSum

listSum :: (Foldable t, Num a) => t a -> a
listSum = foldl add 0 

add :: Num a => a -> a -> a
add a b = a + b

-- 4 listSumPerimeter

listSumPerimeter :: Num a => [(a,a,a,a)] -> a
listSumPerimeter l = listSum (listOfSidesToPerimeter l)

listOfSidesToPerimeter :: Num a => [(a,a,a,a)] -> [a]
listOfSidesToPerimeter = map findPerimeterFromTuple

findPerimeterFromTuple :: Num a => (a,a,a,a) -> a
findPerimeterFromTuple (a,b,c,d) = findPerimeter a b c d 

-- 5 reverseList

reverseList :: [a] -> [a]
reverseList = reverse

-- 6 insertionSortMaxSideAsc

insert :: Ord a => a -> (a -> a -> Bool) -> [a] -> [a]
insert x f [] = [x]
insert x f (y:ys) 
    | f x y = x:y:ys
    | otherwise = y : insert x f ys

insertionSort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
insertionSort f [x] = [x]
insertionSort f (x:xs) = insert x f (insertionSort f xs)

insertionSortMaxSideAsc :: (Num a, Ord a) => [(a,a,a,a)] -> [(a,a,a,a)]
insertionSortMaxSideAsc = insertionSort sortMaxSide

sortMaxSide :: (Num a, Ord a) => (a,a,a,a) -> (a,a,a,a) -> Bool
sortMaxSide l1 l2 = maxSide l1 < maxSide l2

maxSide :: (Num a, Ord a) => (a,a,a,a) -> a
maxSide (a,b,c,d)
    | a >= b && a >= c && a >= d = a
    | b >= c && b >= d = b
    | c >= d = c
    | otherwise = d

-- 7 insertionSortPerimeterDesc

insertionSortPerimeterDesc :: (Num a, Ord a) => [(a,a,a,a)] -> [(a,a,a,a)]
insertionSortPerimeterDesc = insertionSort sortPerimeter

sortPerimeter :: (Num a, Ord a) => (a,a,a,a) -> (a,a,a,a) -> Bool
sortPerimeter l1 l2 = findPerimeterFromTuple l1 > findPerimeterFromTuple l2

-- 8 filterEven

filterEven :: (Num a, Ord a) => [a] -> [a]
filterEven = filter isEven

isEven :: (Num a, Ord a) => a -> Bool
isEven 0 = True
isEven n = isOdd (n-1)

isOdd :: (Num a, Ord a) => a -> Bool
isOdd 0 = False
isOdd n = isEven (n-1)

-- 9 filterRectangle

filterRectangle :: Eq a => [(a,a,a,a)] -> [(a,a,a,a)]
filterRectangle = filter isRectangleFromTuple

isRectangleFromTuple :: Eq a => (a,a,a,a) -> Bool
isRectangleFromTuple (a,b,c,d) = isRectangle a b c d

-- 10 filterNotRectangle

filterNotRectangle :: Eq a => [(a,a,a,a)] -> [(a,a,a,a)]
filterNotRectangle = filter isNotRectangleFromTuple

isNotRectangleFromTuple :: Eq a => (a,a,a,a) -> Bool
isNotRectangleFromTuple (a,b,c,d) = not (isRectangle a b c d)
