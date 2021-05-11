{-# LANGUAGE DeriveFunctor, DeriveFoldable, InstanceSigs #-}

module Primary.Traversable (reciprocal, reciprocal', tree0, tree1) where

import Data.Traversable ()

class (Functor t, Foldable t) => TraversableSelf t where
  traverseS :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverseS f = sequenceAS . fmap f
  sequenceAS :: Applicative f => t (f a) -> f (t a)
  sequenceAS = traverseS id
  {-# MINIMAL traverseS | sequenceAS #-}

reciprocal :: [Double] -> Maybe [Double]
reciprocal = traverse (\x -> if x == 0 then Nothing else Just (1 / x))

data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving (Show, Foldable, Functor)


instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf a) = fmap Leaf (f a)
  traverse f (Node l a r) = Node <$> traverse f l <*> f a <*> traverse f r
  sequenceA :: Applicative f => Tree (f a) -> f (Tree a)
  sequenceA (Leaf a) = Leaf <$> a
  sequenceA (Node l a r) = Node <$> sequenceA l <*> a <*> sequenceA r

tree0, tree1 :: Tree Double
tree0 = Node (Leaf 0) 1 (Leaf 2)
tree1 = Node (Leaf 1) 2 (Leaf 3)

reciprocal' :: (Eq b, Fractional b, Traversable t) => t b -> Maybe (t b)
reciprocal' = traverse (\x -> if x == 0 then Nothing else Just (1/x))


newtype Identity a = Identity { runIdentity :: a }
newtype Constant a b = Constant { getConstant :: a }

instance Functor (Constant a) where
  fmap f (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap :: Monoid m => (b -> m) -> Constant a b -> m
  foldMap f (Constant x) = mempty

instance (Monoid a) => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant x <*> Constant y = Constant (x `mappend` y)

instance Traversable (Constant a) where
  traverse :: Applicative f => (a1 -> f b) -> Constant a a1 -> f (Constant a b)
  traverse f (Constant a) = pure (Constant a)

