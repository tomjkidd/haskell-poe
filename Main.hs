module Main where

import Data.List
import System.Environment
import qualified BasicInputOutput as BIO
import qualified SimpleGraphics as SG

main1 :: IO ()
main1 = SG.polybezierDemo

data Demo = Demo {
  name :: String,
  eval :: IO ()
}

demos :: [Demo]
demos = 
  [
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
