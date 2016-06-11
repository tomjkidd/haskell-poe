module Draw (inchToPixel, pixelToInch, intToFloat,
             xWin, yWin, trans, shapeToGraphic,
             shapeDemo1, shapeDemo2, shapeDemo3, shapeDemo4
            )where

import Shape
import Common
import SOE

pixelsPerInch :: Float
pixelsPerInch = 100

inchToPixel :: Float -> Int
inchToPixel x = round (pixelsPerInch * x)

pixelToInch :: Int -> Float
pixelToInch n = intToFloat n/pixelsPerInch

intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

xWin, yWin :: Int
xWin = 600
yWin = 500

xWin2, yWin2 :: Int
xWin2 = xWin `div` 2
yWin2 = yWin `div` 2

trans :: Vertex -> Point
trans (x,y) = (xWin2 + inchToPixel x,
               yWin2 - inchToPixel y)

transList :: [Vertex] -> [Point]
transList vs = map trans vs

shapeToGraphic :: Shape -> Graphic
shapeToGraphic (Rectangle s1 s2)
  = let s12 = s1/2
        s22 = s2/2
    in polygon (transList [(-s12, -s22), (-s12, s22), (s12, s22), (s12, -s22)])
shapeToGraphic (Ellipse r1 r2)
  = ellipse (trans (-r1, -r2)) (trans (r1, r2))
shapeToGraphic (RtTriangle s1 s2)
  = polygon (transList [(0,0), (s1, 0), (0, s2)])
shapeToGraphic (Polygon vts)
  = polygon (transList vts)

sh1, sh2, sh3, sh4 :: Shape
sh1 = Rectangle 3 2
sh2 = Ellipse 1 1.5
sh3 = RtTriangle 3 2
sh4 = Polygon [(-2.5, 2.5), (-1.5, 2.0), (-1.1, 0.2), (-1.7, -1.0), (-3.0, 0)]

shapeDemo1 :: IO ()
shapeDemo1
  = runGraphics (
  do w <- openWindow "Shape Demo 1" (xWin, yWin)
     drawInWindow w (withColor Red (shapeToGraphic sh1))
     drawInWindow w (withColor Blue (shapeToGraphic sh2))
     spaceClose w
  )

type ColoredShapes = [(Color, Shape)]

shapes :: ColoredShapes
shapes = [(Red, sh1), (Blue, sh2), (Yellow, sh3), (Magenta, sh4)]

drawShapes :: Window -> ColoredShapes -> IO ()
drawShapes _ [] = return ()
drawShapes w css =
  sequence_ $ map drawShape css
  where drawShape (c,s) = drawInWindow w (withColor c (shapeToGraphic s))

shapeDemo2 :: IO ()
shapeDemo2
  = runGraphics (
    do w <- openWindow "Shape Demo 2" (xWin, yWin)
       drawShapes w shapes
       spaceClose w
    )

shapeDemo3 :: IO ()
shapeDemo3
  = runGraphics (
    do w <- openWindow "Shape Demo 3" (xWin, yWin)
       drawShapes w [
         (Red, Rectangle 5.5 4.5),
         (Blue, Ellipse 3 2.5),
         (Cyan, Ellipse 3 2),
         (Yellow, RtTriangle (-3) (-2)),
         (White, RtTriangle 3 (-2)),
         (Magenta, Polygon [(-3, 0), (0, 3), (3, 0)]),
         (Black, Polygon [(-2, 0), (0, 2), (2, 0)]),
         (Green, Polygon [(-1, 0), (0, 1), (1, 0)])]
       spaceClose w
    )

conCircles :: [Shape]
conCircles = map circle [2.4, 2.1..0.3]

coloredCircles :: [(Color, Shape)]
coloredCircles =
  zip [Black, Blue, Green, Cyan, Red, Magenta, Yellow, White] conCircles

shapeDemo4 :: IO ()
shapeDemo4 =
  runGraphics $ do w <- openWindow "Shape Demo 4" (xWin, yWin)
                   drawShapes w coloredCircles
                   spaceClose w
