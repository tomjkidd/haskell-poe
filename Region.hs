-- | A library to work with combining Shapes

module Region (Region (Shape, Translate, Scale, Complement,
                       Union, Intersect, Empty),
                Coordinate,
                containsS, containsR,
                module Shape
              ) where

import Shape

data Region = Shape Shape
            | Translate Vector Region
            | Scale Vector Region
            | Complement Region
            | Region `Union` Region
            | Region `Intersect` Region
            | Empty
  deriving Show

type Vector = (Float, Float)

type Coordinate = (Float, Float)

infixr 5 `Union`
infixr 6 `Intersect`

containsS :: Shape -> Coordinate -> Bool
(Rectangle s1 s2) `containsS` (x,y) =
  let t1 = s1/2
      t2 = s2/2
  in -t1 <= x && x <= t1 && -t2 <= y && y <= t2
(Ellipse r1 r2) `containsS` (x,y) =
  (x/r1)^2 + (y/r2)^2 <= 1
(Polygon pts) `containsS` p =
  let leftOfList = map isLeftOfp
                   (zip pts (tail pts ++ [head pts]))
      isLeftOfp p' = isLeftOf p p'
  in and leftOfList
(RtTriangle s1 s2) `containsS` p =
  (Polygon [(0, 0), (s1, 0), (0, s2)]) `containsS` p

isLeftOf :: Coordinate -> Ray -> Bool
(px, py) `isLeftOf` ((ax, ay), (bx, by)) =
  let (s, t) = (px - ax, py - ay)
      (u, v) = (px - bx, py - by)
  in s * v >= t * u

type Ray = (Coordinate, Coordinate)

containsR :: Region -> Coordinate -> Bool
(Shape s) `containsR` p = s `containsS` p
(Translate (u,v) r) `containsR` (x, y) = r `containsR` (x - u, y - v)
(Scale (u,v) r) `containsR` (x, y) = r `containsR` (x/u, y/v)
(Complement r) `containsR` p = not (r `containsR` p)
Empty `containsR` p = False
(r1 `Union` r2) `containsR` p = r1 `containsR` p || r2 `containsR` p
(r1 `Intersect` r2) `containsR` p = r1 `containsR` p && r2 `containsR` p

demo =
  let circle = Shape (Ellipse 0.5 0.5)
      square = Shape (Rectangle 1 1)
  in (Scale (2,2) circle)
     `Union` (Translate (1, 0) square)
     `Union` (Translate (-1, 0) square)

oneCircle = Shape (Ellipse 1 1)
manyCircles = [Translate (x, 0) oneCircle | x <- [0, 2..]]
fiveCircles = foldr Union Empty (take 5 manyCircles)
