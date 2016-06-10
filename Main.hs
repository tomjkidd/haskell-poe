module Main where

import SOE

spaceClose :: Window -> IO()
spaceClose w = do k <- getKey w
                  if k == ' '
                    then closeWindow w
                    else spaceClose w

pic1 = withColor Red (ellipse (150, 150) (300, 200))

pic2 = withColor Blue (polyline [(100, 50), (200, 50), (200, 250), (100, 250), (100, 50)])

main1 :: IO ()
main1 = runGraphics (
  do w <- openWindow "First Graphics Program" (300,300)
     drawInWindow w (withColor White (text (100, 200) "HelloGraphicsWorld"))
     drawInWindow w pic1
     drawInWindow w pic2
     k <- getKey w
     closeWindow w
  )
  

main2 :: IO ()
main2 = runGraphics (
  do w <- openWindow "First Graphics Program" (300,300)
     drawInWindow w (text (100, 200) "HelloGraphicsWorld")
     spaceClose w
     closeWindow w
  )

fillTri :: Window -> Int -> Int -> Int -> IO ()
fillTri w x y size = drawInWindow w (withColor Blue (polygon [(x, y), (x + size, y), (x, y - size), (x, y)]))

minSize :: Int
minSize = 8

sierpinskiTri :: Window -> Int -> Int -> Int -> IO ()
sierpinskiTri w x y size =
  if size <= minSize
  then fillTri w x y size
  else let size2 = size `div` 2
       in do sierpinskiTri w x y size2
             sierpinskiTri w x (y - size2) size2
             sierpinskiTri w (x + size2) y size2

main3 = runGraphics (
  do w <- openWindow "Sierpinski's Triangle" (400, 400)
     sierpinskiTri w 50 300 256
     spaceClose w
  )

main :: IO ()
main = main3
