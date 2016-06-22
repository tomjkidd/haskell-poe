module Main where

import Data.List
import System.Environment
import qualified BasicInputOutput as BIO
import qualified SimpleGraphics as SG
import Draw
import Picture
import Animation

data Demo = Demo {
  name :: String,
  eval :: IO ()
}

demos :: [Demo]
demos = 
  [
    -- ----------------------
    -- BasicInputOutput Demos
    -- ----------------------
    Demo
    { name = "sequence",
      eval = BIO.sequenceDemo
    },

    -- --------------------
    -- SimpleGraphics Demos
    -- --------------------
    Demo
    { name = "line",
      eval = SG.lineDemo
    },

    Demo
    { name = "polyline",
      eval = SG.polylineDemo
    },

    Demo
    { name = "polybezier",
      eval = SG.polybezierDemo
    },

    Demo
    { name = "polygon",
      eval = SG.polygonDemo
    },

    Demo
    { name = "ellipse",
      eval = SG.ellipseDemo
    },
    
    Demo
    { name = "shearellipse",
      eval = SG.shearEllipseDemo
    },

    -- --------------------
    -- Draw Demos
    -- --------------------
    Demo
    { name = "draw1",
      eval = Draw.shapeDemo1
    },

    Demo
    { name = "draw2",
      eval = Draw.shapeDemo2
    },

    Demo
    { name = "draw3",
      eval = Draw.shapeDemo3
    },

    Demo
    { name = "draw4",
      eval = Draw.shapeDemo4
    },

    -- --------------------
    -- Picture Demos
    -- --------------------
    Demo
    { name = "picture1",
      eval = Picture.demo1
    },

    Demo
    { name = "picture2",
      eval = Picture.demo2
    },

    Demo
    { name = "picture3",
      eval = Picture.demo3
    },

    Demo
    { name = "picture4",
      eval = Picture.demo4
    },

    Demo
    { name = "picture5",
      eval = Picture.demo5
    },

    Demo
    { name = "picture6",
      eval = Picture.demo6
    },

    Demo
    { name = "picture7",
      eval = Picture.demo7
    },

    -- --------------------
    -- Animation Demos
    -- --------------------
    Demo
    { name = "animation1",
      eval = Animation.demo1
    },

    Demo
    { name = "animation2",
      eval = Animation.demo2
    },

    Demo
    { name = "animation3",
      eval = Animation.demo3
    },

    Demo
    { name = "animation4",
      eval = Animation.demo4
    }
  ]

main :: IO ()
main = do  
   args <- getArgs  
   progName <- getProgName  
   putStrLn "The arguments are:"  
   mapM putStrLn args  
   putStrLn "The program name is:"  
   putStrLn progName
   if length args > 0
     then case Data.List.find (\x -> name x == (args!!0)) demos of
            Just x -> eval x
            Nothing -> SG.main3
     else SG.main3
