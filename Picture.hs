-- | A module for drawing regions

module Picture (Picture (Region, Over, EmptyPic),
                Color (Black, Blue, Green, Cyan, Red, Magenta, Yellow, White),
                regionToGRegion, shapeToGRegion,
                drawRegionInWindow, drawPic, draw,
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
                        yWin2 + inchToPixel (ly + y * sy))

draw :: String -> Picture -> IO ()
draw s p = runGraphics $ do w <- openWindow s (xWin, yWin)
                            drawPic w p
                            spaceClose w
