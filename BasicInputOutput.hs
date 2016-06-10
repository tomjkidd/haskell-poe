module BasicInputOutput where

{- This code comes from Chapter 3 -}

-- NOTE: IO () is the type used when a command has no useful value to return.
-- NOTE: () is the unit type. It can be thought of as a noop.
main :: IO ()
main = putStr "Testing. 1... 2... 3.\n"

file :: String
file = "bio-ignore.txt"

writeAndDisplay :: String ->  IO ()
writeAndDisplay s =
  do writeFile file s
     putStr ("Wrote to file: " ++ s)

readInputWriteAndDisplay :: IO ()
readInputWriteAndDisplay =
  do putStr "Enter a line to add to bio-ignore.txt"
     l <- getLine
     writeAndDisplay l

cat :: IO ()
cat =
  do name <- getLine
     s <- readFile name
     putStr s

exampleSequence :: [IO ()]
exampleSequence = [putStr "a\n", putStr "b\n", putStr "c\n"]

sequenceDemo :: IO ()
sequenceDemo = sequence_ exampleSequence
