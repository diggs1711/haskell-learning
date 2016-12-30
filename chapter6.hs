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

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter':: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldr1 (\_ x -> x)

sqrtSum :: Int 
sqrtSum = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
