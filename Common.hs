module Common where

import SOE

spaceClose :: Window -> IO()
spaceClose w = do k <- getKey w
                  if k == ' '
                    then closeWindow w
                    else spaceClose w