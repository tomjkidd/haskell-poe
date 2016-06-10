# Purpose

This repo is meant to keep track of the work done for [The Haskell School of Expression](http://www.cs.yale.edu/homes/hudak/SOE/)

## Important

The website instructs you to download a zipfile for examples, as well as the SOE module.
The SOE module contains an error on line 488, the line with 'if ch == '\x0' then return ch'.
I discovered this error trying to close the window using the space bar, using the spaceClose
function on p. 41 of the book.

To fix this issue, the if should use /= (not equal), and my version in this repository has 
been changed.

```haskell
getKey win = do
  ch <- getKeyEx win True
  if ch == '\x0' then return ch -- The == here should be /=!
    else getKeyEx win False
```

# Useful Commands
    
Use [GHCi](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html)
```shell
ghci
```
    
Load Shape module (in GHCi)  
```haskell
:load Shape
```

Build main
```shell
ghc -o main Main.hs
```
