module Main where

import Data.List
import System.Environment
import qualified BasicInputOutput as BIO
import qualified SimpleGraphics as SG
import Draw

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
