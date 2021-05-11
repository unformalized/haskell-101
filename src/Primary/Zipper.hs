module Primary.Zipper () where

data Zipper a = Zipper [a] a [a] deriving Show

fromList :: [a] -> Zipper a
fromList (x:xs) = Zipper [] x xs
fromList _      = error "empty!"


-- 向后遍历
next :: Zipper a -> Zipper a
next (Zipper ys y (x:xs)) = Zipper (y:ys) x xs
next z = z

-- 向前遍历
prev :: Zipper a -> Zipper a
prev (Zipper (y:ys) x xs) = Zipper ys y (x:xs)
prev z = z

data Tree a = Leaf a | Node a (Tree a) (Tree a)
data Accumulate a = Empty
                  | L (Accumulate a) a (Tree a)
                  | R (Accumulate a) a (Tree a)

-- 使用 Zipper 遍历树
type ZipperT a = (Tree a, Accumulate a)

right, left, up :: ZipperT a -> ZipperT a
right (Node n l r, a) = (r, R a n l)
right a = a

left (Node n l r, a) = (l, L a n r)
left a = a

up (t, R a n l) = (Node n l t, a)
up (t, L a n r) = (Node n t r, a)
up z@(t, Empty) = z

-- 
data Branch a = RB a (Tree a) | LB a (Tree a)

type ZipperB a = (Tree a, [Branch a])

rightB, leftB, upB :: ZipperB a -> ZipperB a

rightB (Node n l r, t) = (r, RB n l:t)
rightB z@(Leaf a, t) = z

leftB (Node n l r, t) = (l, LB n r:t)
leftB z@(Leaf a, t) = z

upB (r, (RB n l):rest) = (Node n l r, rest)
upB (l, (LB n r):rest) = (Node n l r, rest)
upB z@(t, []) = z

