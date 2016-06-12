module Exercises where

{- Exercise 5.3 -}
xss :: [[Int]]
xss = [[], [1], [1,2], [1,2,3], [1,2,3,4]]

-- foldr :: (a -> b -> b) -> b -> [a] -> b
length :: [a] -> Int
length xs = foldr (\_ acc -> 1 + acc) 0 xs

exercise5_3 :: [Int]
exercise5_3 = map Exercises.length xss

{- Exercise 5.4 -}
{- f1 (f2 (*) [1,2,3,4]) 5 => [5,10,15,20] -}

f2 :: (a -> (a -> a)) -> [a] -> [(a -> a)]
f2 op xs = map (\x -> op x) xs

f1 :: [(a -> a)] -> a -> [a]
f1 fs n = map (\f -> f n) fs

exercise5_4 :: [Int]
exercise5_4 = f1 (f2 (*) [1,2,3,4]) 5
