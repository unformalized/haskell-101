module Primary.Hof (
    rationalNumber,
    (|>),
    (>>-)
) where

import Prelude hiding (scanl)

-- scanl

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f a [] = [a]
scanl f a (x:xs) = a : scanl f (f a x) xs

interLeave :: [a] -> [a] -> [a]
interLeave [] xs = xs
interLeave (x:xs) ys = x:interLeave ys xs

interLeaveList :: [[a]] -> [a]
interLeaveList = foldr interLeave []


rationalNumber :: [(Integer, Integer)]
rationalNumber = interLeaveList [[(x,y) | y <- [1..]] | x <- [1..]]


mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumL _ s [] = (s, [])
mapAccumL f s (x:xs) = (s'', y : ys)
    where
        (s', y) = f s x
        (s'', ys) = mapAccumL f s' xs

mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumR _ s [] = (s, [])
mapAccumR f s (x:xs) = (s'', y : ys)
    where
        (s', ys) = mapAccumR f s xs
        (s'', y) = f s' x


infix 9 >>-

(>>-) :: (a -> b) -> (b -> c) -> (a -> c)
(>>-) = flip (.)


(|>) :: b -> (b -> c) -> c
(|>) = flip ($)


-- foldl :: (b -> a -> b) -> [a] -> b -> b
-- foldr :: (a -> b -> b) -> [a] -> b -> b
-- foldl const :: 


