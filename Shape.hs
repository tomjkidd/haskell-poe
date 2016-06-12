module Shape where

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
  deriving Show

type Radius = Float
type Side = Float
type AngleInDegrees = Float
type Vertex = (Float, Float)

square  :: Side -> Shape
square s = Rectangle s s

circle :: Side -> Shape
circle r = Ellipse r r

{- Exercise 2.1 -}
rectangle :: Side -> Side -> Shape
rectangle x y = Polygon [(0,0), (x,0), (x,y), (0,y)]

rtTriangle :: Side -> Side -> Shape
rtTriangle b h = Polygon [(0,0), (b,0), (b,h)]

{- Exercise 2.2 -}
{- NOTE: Polygon is oriented so that first vertex is at (0,0). The inner angle is used to figure out how to construct
   the polygon in a point-by-point manner -}
regularPolygon :: Int -> Side -> Shape
regularPolygon n s = Polygon (regularPolygonVertices n s)

regularPolygonVertices :: Int -> Side -> [Vertex]
regularPolygonVertices n s = let innerAngle = (180 - 360/ (fromIntegral n))
                                 delta = 180 - innerAngle -- The angle to increment by with correct cartesian orientation
                                 vertices = regularPolygonHelper (n-1) 0 delta s [(0,0)]
                             in vertices

regularPolygonHelper :: Int -> AngleInDegrees -> AngleInDegrees -> Radius -> [Vertex] -> [Vertex]
regularPolygonHelper n cur delta r vs =
  if n == 0
  then vs
  else
    let newVertex = polarAdd (last vs) cur r
        newVertices = vs ++ [newVertex]
        nextAngle = cur + delta
    in regularPolygonHelper (n-1) nextAngle delta r newVertices
                                          

distBetween :: Vertex -> Vertex -> Float
distBetween (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

{- Generate a new Vertex given the current vertex, the current angle, the distance between the vertices, and direction to go in  -}
polarAdd :: Vertex -> AngleInDegrees -> Radius -> Vertex
polarAdd (x1, y1) d r = let theta = d * (pi/180)
                            newX = x1 + r * cos theta
                            newY = y1 + r * sin theta
                        in (newX, newY)

triArea :: Vertex -> Vertex -> Vertex -> Float
triArea v1 v2 v3 = let a = distBetween v1 v2
                       b = distBetween v2 v3
                       c = distBetween v3 v1
                       s = 0.5 * (a + b + c)
                   in
                     sqrt (s * (s-a) * (s-b) * (s-c))

polygonAreaV1 :: [Vertex] -> Float
polygonAreaV1 (v1:v2:v3:vs) = triArea v1 v2 v3 + area (Polygon (v1:v3:vs))
polygonAreaV1 _ = 0


area :: Shape -> Float
area (Rectangle s1 s2)
  | s1 >= 0 && s2 >= 0 = s1 * s2
  | otherwise = error "area: negative side lengths"
area (Ellipse r1 r2) = pi * r1 * r2
area (RtTriangle s1 s2) = 1/2 * s1 * s2
area (Polygon vs) = polygonAreaV1 vs
