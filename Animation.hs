-- | A module for animation of Regions and Shapes

module Animation where

import Shape
import Draw
import Picture
import SOE hiding (Region)
import qualified SOE as G (Region)
import Common

type Animation a = Time -> a
type Time = Float

rubberBall :: Animation Shape
rubberBall t = Ellipse (sin t) (cos t)

revolvingBall :: Animation Region
revolvingBall t =
  let ball = Shape (Ellipse 0.2 0.2)
  in Translate (sin t, cos t) ball

planets :: Animation Picture
planets t =
  let p1 = Region Red (Shape (rubberBall t))
      p2 = Region Yellow (revolvingBall t)
  in p1 `Over` p2

tellTime :: Animation String
tellTime t = "The time is " ++ show t

animate :: String -> Animation Graphic -> IO ()
animate title anim =
  runGraphics $
  do w <- openWindowEx title (Just (0,0)) (Just (xWin,yWin))
            drawBufferedGraphic --(Just 30)
     t0 <- timeGetTime
     let loop =
           do t <- timeGetTime
              let ft = intToFloat (word32ToInt (t-t0))/1000
              setGraphic w (anim ft)
              spaceCloseEx w loop
     loop

regionToGraphic :: Region -> Graphic
regionToGraphic = drawRegion . regionToGRegion

pictureToGraphic :: Picture -> Graphic
pictureToGraphic (Region c r) = withColor c (regionToGraphic r)
pictureToGraphic (p1 `Over` p2) =
  pictureToGraphic p1 `overGraphic` pictureToGraphic p2
pictureToGraphic EmptyPic = emptyGraphic

demo1 :: IO ()
demo1 = animate "Animated Shape"
  (withColor Blue . shapeToGraphic . rubberBall)

demo2 :: IO ()
demo2 = animate "Animated Text" (text (100,200) . tellTime)

demo3 :: IO ()
demo3 = animate "Animated Region" (withColor Yellow . regionToGraphic . revolvingBall)

demo4 :: IO ()
demo4 = animate "Animated Picture" (pictureToGraphic . planets)
