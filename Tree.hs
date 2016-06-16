module Tree where

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving Show
-- | Map an operation over the tree
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Branch l r) = Branch (mapTree f l) (mapTree f r)

-- | Gather the leaves of the tree as a list
fringe :: Tree a -> [a]
fringe (Leaf x) = [x]
fringe (Branch l r) = fringe l ++ fringe r

-- | Compute the size of a tree (number of leaves)
treeSize :: Tree a -> Integer
treeSize (Leaf _) = 1
treeSize (Branch l r) = treeSize l + treeSize r

-- | Compute the height of a tree (leaf is 0 height)
treeHeight :: Tree a -> Integer
treeHeight (Leaf _) = 0
treeHeight (Branch l r) = 1 + (max (treeHeight l) (treeHeight r))
