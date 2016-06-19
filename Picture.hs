-- | A module for drawing regions

module Picture (Picture (Region, Over, EmptyPic),
                Color (Black, Blue, Green, Cyan, Red, Magenta, Yellow, White),
                regionToGRegion, shapeToGRegion,
                drawRegionInWindow, drawPic, draw,
                demo1, demo2, demo3, demo4, demo5, demo6, demo7,
                module Region
               ) where

import Draw
import Region
import Common
import SOE hiding (Region)
import qualified SOE as G (Region) -- G.Regions are graphics regions, which are the pixels sets for regions

data Picture = Region Color Region
             | Picture `Over` Picture
             | EmptyPic
  deriving Show

drawRegionInWindow :: Window -> Color -> Region -> IO ()
drawRegionInWindow w c r = drawInWindow w (withColor c (drawRegion (regionToGRegion r)))

drawPic :: Window -> Picture -> IO ()
drawPic w (Region c r) = drawRegionInWindow w c r
drawPic w (p1 `Over` p2) = do drawPic w p2;drawPic w p1
drawPic _ EmptyPic = return ()

regionToGRegion :: Region -> G.Region
regionToGRegion r = regToGReg (0, 0) (1, 1) r

regToGReg :: Vector -> Vector -> Region -> G.Region
regToGReg loc sca (Shape s) = shapeToGRegion loc sca s
regToGReg loc (sx, sy) (Scale (u, v) r) = regToGReg loc (sx * u, sy * v) r
regToGReg (lx, ly) (sx, sy) (Translate (u,v) r) = regToGReg (lx + u * sx, ly + v * sy) (sy, sy) r
regToGReg _ _ Empty = createRectangle (0, 0) (0, 0)
regToGReg loc sca (r1 `Union` r2) =
  primGReg loc sca r1 r2 orRegion
regToGReg loc sca (r1 `Intersect` r2) =
  primGReg loc sca r1 r2 andRegion
regToGReg loc sca (Complement r) =
  primGReg loc sca winRect r diffRegion

winRect :: Region
winRect = Shape (Rectangle (pixelToInch xWin) (pixelToInch yWin))


primGReg :: Vector -> Vector -> Region -> Region -> (G.Region -> G.Region -> a) -> a
primGReg loc sca r1 r2 op =
  let gr1 = regToGReg loc sca r1
      gr2 = regToGReg loc sca r2
  in op gr1 gr2

type Vector = (Float, Float)

xWin2, yWin2 :: Int
xWin2 = xWin `div` 2
yWin2 = yWin `div` 2

shapeToGRegion :: Vector -> Vector -> Shape -> G.Region
shapeToGRegion (lx, ly) (sx, sy) s =
  case s of
    Rectangle s1 s2
      -> createRectangle (trans' (-s1/2, -s2/2)) (trans' (s1/2, s2/2))
    Ellipse r1 r2
      -> createEllipse (trans' (-r1, -r2)) (trans' (r1, r2))
    Polygon vs
      -> createPolygon (map trans' vs)
    RtTriangle s1 s2
      -> createPolygon (map trans' [(0, 0), (s1, 0), (0, s2)])
  where trans' :: Vertex -> Point
        trans' (x, y) = (xWin2 + inchToPixel (lx + x * sx),
                        yWin2 - inchToPixel (ly + y * sy))

draw :: String -> Picture -> IO ()
draw s p = runGraphics $ do w <- openWindow s (xWin, yWin)
                            drawPic w p
                            spaceClose w

xUnion :: Region -> Region -> Region
p1 `xUnion` p2 = (p1 `Intersect` Complement p2) `Union`
                 (p2 `Intersect` Complement p1)

r1',r2',r3',r4',reg1,reg2
  ,oneCircle,fiveCircles,boundingRect :: Region
  
manyCircles :: [Region]

pic1,pic2,pic3,pic4,pic5,pic6 :: Picture
r1' = Shape (Rectangle 3 2)
r2' = Shape (Ellipse 1 1.5)
r3' = Shape (RtTriangle 3 2)
r4' = Shape (Polygon [(-2.5, 2.5), (-3.0, 0), (-1.7, -1.0), (-1.1, 0.2), (-1.5, 2.0)])

reg1 = r3' `xUnion` (r1' `Intersect` Complement r2' `Union` r4')
pic1 = Region Blue reg1

reg2 = let circle' = Shape (Ellipse 0.5 0.5)
           square' = Shape (Rectangle 1 1)
       in (Scale (2, 2) circle')
          `Union` (Translate (1, 0) square')
          `Union` (Translate (-1, 0) square')

pic2 = Region Yellow (Translate (0, -1) reg2)

pic3 = pic2 `Over` pic1

oneCircle = Scale (0.25, 0.25) (Shape (Ellipse 1 1))
manyCircles = [Translate ( x * 0.25, 0) oneCircle | x <- [0, 2..]]
fiveCircles = foldr Union Empty (take 5 manyCircles)

boundingRect = (Translate (1, 0) (Shape (Rectangle 2.5 0.5)))

{- Here I divided 360 by 5 to get 72 between each vertex.
By generating the list using [0, 72..] I go around the entire circle.
By adding 18 degrees to each vertex, I can position a vertex on (0, 1).
-}
pentagonPts :: [(Float, Float)]
pentagonPts = let degrees = map (+ 18) $ take 6 [0, 72..]
                  cosXs = map (cos . degreesToRadians) degrees
                  sinXs = map (sin . degreesToRadians) degrees
              in zip cosXs sinXs

pentagon :: Region
pentagon = (Shape (Polygon pentagonPts))

pic4 = Region Magenta fiveCircles `Over` Region Yellow boundingRect

pic5 = Region Magenta (fiveCircles `xUnion` boundingRect)

pic6 = Region Red (Scale (0.5, 0.5) pentagon) `Over` Region Blue pentagon

demo1 :: IO ()
demo1 = draw "pic1" pic1

demo2 :: IO ()
demo2 = draw "pic2" pic2

demo3 :: IO ()
demo3 = draw "pic3" pic3

demo4 :: IO ()
demo4 = draw "pic4" pic4

demo5 :: IO ()
demo5 = draw "pic5" pic5

demo6 :: IO ()
demo6 = draw "pic6" pic6

pictToList :: Picture -> [(Color, Region)]
pictToList EmptyPic = []
pictToList (Region c r) = [(c, r)]
pictToList (p1 `Over` p2) = pictToList p1 ++ pictToList p2

adjust :: [(Color, Region)] -> Coordinate -> (Maybe (Color, Region), [(Color, Region)])
adjust [] _ = (Nothing, [])
adjust regs p =
  case (break (\(_, r) -> r `containsR` p) regs) of
    (top, hit:rest) -> (Just hit, top ++ rest)
    (_, []) -> (Nothing, regs)

loop :: Window -> [(Color, Region)] -> IO ()
loop w regs =
  do clearWindow w
     sequence_ [drawRegionInWindow w c r | (c,r) <- reverse regs]
     (x,y) <- getLBP w
     case (adjust regs (pixelToInch (x - xWin2),
                        pixelToInch (yWin2 - y))) of
       (Nothing, _) -> closeWindow w
       (Just hit, newRegs) -> loop w (hit:newRegs)

draw2 :: String -> Picture -> IO ()
draw2 s p =
  runGraphics $ do w <- openWindow s (xWin, yWin)
                   loop w (pictToList p)

p1',p2',p3',p4' :: Picture
p1' = Region Red r1'
p2' = Region Blue r2'
p3' = Region Green r3'
p4' = Region Yellow r4'

pic :: Picture
pic = foldl Over EmptyPic [p1',p2',p3',p4']

demo7 :: IO ()
demo7 = draw2 "Picture Click Test"  pic
