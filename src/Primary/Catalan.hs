module Primary.Catalan where

data Tree = Leaf | Node Tree Tree deriving Show

trees :: Int -> [Tree]
trees 0 = [Leaf]
trees n = [Node lt rt | l <- [0..(n - 1)], lt <- trees l, rt <- trees (n - 1 - l)]

brace :: Tree -> String
brace Leaf = ""
brace (Node l r) = '(':brace l ++ ")" ++ brace r

brace' :: Int -> [String]
brace' 0 = [""]
brace' n = [ "(" ++ inner ++ ")" ++ right | i <-  [0..(n-1)], inner <- brace' i, right <- brace' (n-1-i) ]
