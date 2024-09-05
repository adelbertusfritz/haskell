import Text.XHtml (base)

main :: IO ()
main = do
    print(findPerimeter 1 2 3 4)
    print(isRectangle 1 4 1 4)
    print(listSum [1, 2, 3, 4])
    print(listSumPerimeter [(1,2,3,4), (1,2,3,4)])
    print(reverseList [1,2,3,4])
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

-- 7 insertionSortPerimeterDesc

-- 8 filterEven

filterEven :: Num a => [a] -> [a]
filterEven = filter isEven

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

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