{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RankNTypes #-}

module Advance.Rank where

applyTuple :: (forall a. [a] -> [a]) -> ([b], [c]) -> ([b], [c])
applyTuple f (xs, ys) = (f xs, f ys)

rank2 :: forall b c. (forall a. [a] -> [a]) -> ([b], [c]) -> ([b], [c])
rank2 f (xs, ys) = (f xs, f ys)

rank3 :: ((forall a. a -> a) -> (Bool, Char)) -> (Char, Bool)
rank3 f = (\x -> (snd x, fst x)) (f id)
