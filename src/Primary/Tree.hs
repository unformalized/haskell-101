module Primary.Tree where

data Tree a = Leaf a | Node a (Tree a) (Tree a)
data Tree' a = Leaf' | Node' a (Tree' a) (Tree' a)
data Tree'' a = Leaf'' a | Node'' (Tree'' a) (Tree'' a)
data MTree a = MNode [a] [MTree a]

data DTree a = DNode
  {
    rootLabel :: a
  , subForest :: Forest a
  }

type Forest a = [DTree a]

