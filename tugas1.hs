import Text.XHtml (base)

main :: IO ()
main = do
    print(findPerimeter 1 2 3 4)
    print(isRectangle 1 4 1 4)
    print(listSum [1, 2, 3, 4])


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

listSumPerimeter :: (Foldable t, Num a) => t (a,a,a,a) -> a
listSumPerimeter= 

addTuple :: Num a => (a,a,a,a) -> (a,a,a,a) -> (a,a,a,a)
addTuple (a,b,c,d) (e,f,g,h) = (a+e, b+f, c+g, d+h)

tupleSum :: Num a => (a,a,a,a) -> a
tupleSum (a,b,c,d) = a + b + c + d 


