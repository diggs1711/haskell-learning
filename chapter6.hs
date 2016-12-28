applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWidth' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWidth' _ [] _ = []
zipWidth' _ _ [] = []
zipWidth' f (x:xs) (y:ys) = f x y : zipWidth' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

largestDivisble :: (Integral a) => a
largestDivisble = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n 
    | even n = n:chain (n `div` 2)
    | odd n  = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain[1..100]))
    where isLong xs = length xs > 15

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0