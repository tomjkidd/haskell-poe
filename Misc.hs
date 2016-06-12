module Misc where

putCharList :: String -> [IO ()]
putCharList [] = []
putCharList s = Misc.map putChar s

-- main = sequence_ $ Misc.putCharList "This is a test\n"

{-
Polymorphic examples
-}
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + Misc.length xs

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

{-
Abstraction
-}
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : Misc.map f xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x:(xs Misc.++ ys)

listSum :: [Float] -> Float
listSum [] = 0
listSum (x:xs) = x + listSum xs

listProd :: [Float] -> Float
listProd [] = 1
listProd (x:xs) = x * listProd xs

{- NOTE: listSum and listProd share a lot of structure. -}
{- Identify the things that are changing:
1. The empty list identity (0 and 1)
2. The operation (+ and *)
3. The function that is called (listSum and listProd)
-}

fold :: (a -> b -> b) -> b -> [a] -> b
fold _ acc [] = acc
fold op acc (x:xs) = x `op` Misc.fold op acc xs

listSum2 :: [Float] -> Float
listSum2 xs = Misc.fold (+) 0 xs

listProd2 :: [Float] -> Float
listProd2 xs = Misc.fold (*) 1 xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc [] = acc
foldr op acc (x:xs) = x `op` Misc.foldr op acc xs

foldl :: (a -> b -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl op acc (x:xs) = Misc.foldl op (x `op` acc) xs

concat :: [[a]] -> [a]
concat xss = Misc.foldr (Misc.++) [] xss

slowConcat :: [[a]] -> [a]
slowConcat xss = Misc.foldl (Misc.++) [] xss

slowReverse :: [a] -> [a]
slowReverse [] = []
slowReverse (x:xs) = slowReverse xs Misc.++ [x]

reverse :: [a] -> [a]
reverse xs = rev [] xs
  where rev acc [] = acc
        rev acc (y:ys) = rev (y:acc) ys

reverse2 :: [a] -> [a]
reverse2 xs = Misc.foldl (:) [] xs
