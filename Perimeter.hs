module Perimeter (perimeter,
                  module Shape
                 ) where

import Shape

perimeter :: Shape  -> Float
perimeter (Rectangle l w) = 2 * (l + w)
perimeter (RtTriangle b h) = b + h + sqrt (b^2 + h^2)
perimeter (Polygon vs) = foldl (+) 0 (sides2 vs)
perimeter (Ellipse r1 r2)
  | r1 > r2 = ellipsePerim r1 r2
  | otherwise = ellipsePerim r2 r1

sides :: [Vertex] -> [Side]
sides []  = []
sides (v:vs) = aux v vs
  where aux v1 (v2:vs') = distBetween v1 v2 : aux v2 vs'
        aux vn [] = distBetween vn v:[]


{- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
The first argument takes an 'a' and a 'b' and give back a 'c'.
The second argument is a list of as
The third argument is a list of bs
The return value is a list of cs

In the case of distBetween, a and b have type Vertex and c has type Float.

The main idea of this algorithm is to create two arrays, rotating the original
modularly one position so that the zip will pair each point with it's next
vertex relative to the shape. Putting the head at the tail allows the side from
the last point back to the first to be included. This is expressed really easily.
-}
sides2 :: [Vertex] -> [Side]
sides2 vs = zipWith distBetween vs (tail vs ++ [head vs])

epsilon = 0.0001 :: Float

--eccentricity = sqrt (r1^2 - r2^2)/r1

nextEl :: Float -> Float -> Float -> Float
nextEl e s i = s * (2 * i -1) * (2 * i - 3) * (e^2)/(4 * i^2)

ellipsePerim :: Float -> Float -> Float
ellipsePerim r1 r2 =
  let e = sqrt (r1^2 - r2^2)/r1
      s = scanl aux (0.25 * e^2)[2..]
      aux s' i = nextEl e s' i
      test x = x > epsilon
      sSum = foldl (+) 0 (takeWhile test s)
  in 2 * r1 * pi * (1 - sSum)
