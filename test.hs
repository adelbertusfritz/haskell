import Data.Char

main = do
    print(manipulateString "abcdefghijklmnopqrstuvwxyz")


dropInvalids :: [Char] -> [Char]
dropInvalids = filter (\x -> isLetter x || isSpace x || isDigit x)
    -- isLetter exists

replace' :: (a -> Bool) -> (a -> a) -> [a] -> [a]
replace' _ _ [] = []
replace' f g (x:xs) =
            if f x
            then g x : replace' f g xs
            else x : replace' f g xs
    -- To replace one value with another, use replace (== a) (const b).

replaceWith :: (a -> Bool) -> a -> [a] -> [a]
replaceWith f b = replace' f (const b)

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = replace' (== a) (const b)
  -- The Eq makes sure you can check for equality.

manipulateString :: [Char] -> [Char]
manipulateString = replace 'A' 'Z' . replace 'a' 'z' . replaceWith isDigit ' ' . replace ' ' '_' . dropInvalids