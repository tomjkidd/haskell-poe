module Common where

import SOE

spaceClose :: Window -> IO ()
spaceClose w = do k <- getKey w
                  if k == ' '
                    then closeWindow w
                    else spaceClose w

spaceCloseEx :: Window -> IO () -> IO ()
spaceCloseEx w loop =
  do k <- maybeGetWindowEvent w
     case k of
       Just (Key c d) | c == ' ' && d -> closeWindow w
       Just Closed -> closeWindow w
       Nothing -> loop
       _ -> spaceCloseEx w loop


degreesToRadians :: Float -> Float
degreesToRadians d = d * (pi/180)

xor :: Bool -> Bool -> Bool
xor x y = not x == y
