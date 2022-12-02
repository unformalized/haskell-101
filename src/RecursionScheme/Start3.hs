{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
module RecursionScheme.Start3
  ( RAlgebra,
    RAlgebra',
    RCoAlgebra,
    para,
    para',
    cata',
    apo,
  )
where

import Control.Arrow (Arrow ((&&&)), ArrowChoice ((|||)), (<<<), (>>>))
import Data.Function ((&))
import RecursionScheme.Start2 (Algebra, ExprF (..), Term (..), args, func, printLit)
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P

type RAlgebra f a = f (Term f, a) -> a

-- (Term f, a) 与 Either (Term f) a 对偶
type RCoAlgebra f a = a -> f (Either (Term f) a)

para :: (Functor f) => RAlgebra f a -> Term f -> a
--- &&& :: a -> (a -> b) -> (a -> c) -> (b, c)
--- old version: out >>> fmap (\t -> (t, para f t)) >>> f
para f = out >>> fmap (id &&& para f) >>> f

type RAlgebra' f a = Term f -> f a -> a

para' :: (Functor f) => RAlgebra' f a -> Term f -> a
para' f t = out t & fmap (para' f) & f t

cata' :: Functor f => Algebra f a -> Term f -> a
cata' f = para' (const f)

fastPretty :: RAlgebra' ExprF Doc
fastPretty _ (LiteralF i) = printLit i
fastPretty _ (IdentF s) = P.text s
fastPretty (In CallF {func = "id"}) CallF {args = [theArg]} = theArg
fastPretty _ (CallF f as) = f <> P.parens (P.cat (P.punctuate ", " as))
fastPretty _ (IndexF it idx) = it <> P.brackets idx
fastPretty _ (UnaryF op it) = P.text op <> it
fastPretty _ (BinaryF l op r) = l <> P.text op <> r
fastPretty _ (ParenF exp) = P.parens exp

para'' :: Functor f => RAlgebra f a -> Term f -> a
para'' f = out >>> fmap fanout >>> f
  where
    fanout = id &&& para'' f

apo :: Functor f => RCoAlgebra f a -> a -> Term f
apo f = In <<< fmap (id ||| apo f) <<< f

apo' :: Functor f => RCoAlgebra f a -> a -> Term f
apo' f = In <<< fmap fanin <<< f
  where
    -- fanin :: Functor f => Either (Term f) a -> Term f
    fanin = either id (apo' f)
