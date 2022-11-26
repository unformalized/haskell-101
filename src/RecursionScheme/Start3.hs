-- |
module RecursionScheme.Start3 where

import Control.Arrow (Arrow ((&&&)), (>>>))
import Data.Function ((&))
import RecursionScheme.Start2 (Algebra, Term (..))

type RAlgebra f a = f (Term f, a) -> a

para :: (Functor f) => RAlgebra f a -> Term f -> a
--- &&& :: (a -> b) -> (a -> c) -> (b, c)
--- old version: out >>> fmap (\t -> (t, para f t)) >>> f
para f = out >>> fmap (id &&& para f) >>> f

type RAlgebra' f a = Term f -> f a -> a

para' :: (Functor f) => RAlgebra' f a -> Term f -> a
para' f t = out t & fmap (para' f) & f t

cata' :: Functor f => Algebra f a -> Term f -> a
cata' f = para' (const f)
