-- | A library to work with combining Shapes

module Region (Region (Shape, Translate, Scale, Complement,
                       Union, Intersect, Empty),
                Coordinate,
                containsS, containsR, determineOrientation,
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
  (x/r1)^(2::Integer) + (y/r2)^(2::Integer) <= 1
(Polygon pts) `containsS` p =
  let pts' = case determineOrientation pts of
               Clockwise -> reverse pts
               CounterClockwise -> pts
      leftOfList = map (isLeftOf p)
                   (zip pts' (tail pts' ++ [head pts']))
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
Empty `containsR` _ = False
(r1 `Union` r2) `containsR` p = r1 `containsR` p || r2 `containsR` p
(r1 `Intersect` r2) `containsR` p = r1 `containsR` p && r2 `containsR` p

data Orientation = Clockwise | CounterClockwise
  deriving Show

{- Did a little search on SO and followed the answer provided there

http://stackoverflow.com/questions/1165647/how-to-determine-if-a-list-of-polygon-points-are-in-clockwise-order-}
determineOrientation :: [Coordinate] -> Orientation
determineOrientation ps =
  let qs = tail ps ++ [head ps]
      intermediate = zipWith (\(x1,y1) (x2,y2) -> (x2 - x1) * (y2 + y1)) ps qs
      result = sum intermediate
  in if result >= 0 then Clockwise
     else CounterClockwise
