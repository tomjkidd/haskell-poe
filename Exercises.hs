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

{- Exercise 5.5 -}
{- NOTE: Skipping recursive definitions -}
doubleEach :: [Int] -> [Int]
doubleEach xs = map (\x -> 2 * x) xs

pairAndOne :: [Int] -> [(Int,Int)]
pairAndOne xs = map (\x -> (x, x+1)) xs

addEachPair :: [(Int,Int)] -> [Int]
addEachPair xs = map (\(x,y) -> x + y) xs

exercise5_5 :: ([Int], [(Int, Int)], [Int])
exercise5_5 = (doubleEach [1,2,3], pairAndOne [1,2,3], addEachPair [(1,2), (3,4), (5,6)])

{- Exercise 5.6 -}

maxList :: [Int] -> Int
maxList [] = error "maxList can not be use on []"
maxList xs = foldr (\x acc -> max x acc) (head xs) xs

minList :: [Int] -> Int
minList [] = error "minList can not be used on []"
minList xs = foldr (\x acc -> min x acc) (head xs) xs

{- Exercise 5.7 -}
addPairsPointwise :: [(Int, Int)] -> (Int, Int)
addPairsPointwise xs = foldr (\(x,y) (accx, accy) -> (x + accx, y + accy)) (0,0) xs

exercise5_7 :: (Int, Int)
exercise5_7 = addPairsPointwise [(1,2), (3,4), (5,6)]
