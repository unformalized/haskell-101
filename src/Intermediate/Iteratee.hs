{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Intermediate.Iteratee where

import Data.Function (fix)
import Control.Monad
import qualified Control.Exception as Exc
import Control.Monad.IO.Class
import Control.Monad.Trans

-- define Stream DataType
data Stream a = Chunks [a] | EOF deriving (Show, Functor)

-- impl Monoid, Applicative, Monad typeclass instance similar list
instance Semigroup (Stream a) where
  (<>) (Chunks xs) (Chunks ys) = Chunks (xs ++ ys)
  (<>) _ _ = EOF

instance Monoid (Stream a) where
  mempty = Chunks mempty
  mappend = (<>)

instance Applicative Stream where
  pure = return
  (<*>) = ap

instance Monad Stream where
  return = Chunks . return
  Chunks xs >>= f = mconcat (fmap f xs)
  EOF >>= _ = EOF

-- define Step, Iteratee

data Step a m b
  = Continue (Stream a -> Iteratee a m b) -- need more inputs (Stream a)
  | Yield b (Stream a) -- result: b, rest input: Stream a
  | Error Exc.SomeException -- exception
  deriving Functor

newtype Iteratee s m a = Iteratee { runIteratee :: m (Step s m a) }

deriving instance Functor m => Functor (Iteratee s m)

-- step -> iteratee
returnI :: Monad m => Step a m b -> Iteratee a m b
returnI step = Iteratee (return step)

yield :: Monad m => b -> Stream a -> Iteratee a m b
yield b extra = returnI (Yield b extra)

continue :: Monad m => (Stream a -> Iteratee a m b) -> Iteratee a m b
continue f = returnI (Continue f)

throwError :: (Monad m, Exc.Exception e) => e -> Iteratee a m b
throwError e = returnI (Error (Exc.toException e))

-- impl Iteratee Applicative, Monad typeclass instance


instance Monad m => Applicative (Iteratee a m) where
  pure = return
  (<*>) = ap


-- WTF
instance Monad m => Monad (Iteratee a m) where
  return b = returnI (Yield b (Chunks []))
  m0 >>= f =
    ($ m0) $
    fix $ \bind m ->
            Iteratee $
            runIteratee m >>=
              \case
                Continue k -> return (Continue (bind . k))
                Yield x (Chunks []) -> runIteratee (f x)
                Yield x extra ->
                  runIteratee (f x) >>=
                    \case
                      Continue k' -> runIteratee (k' extra)
                      Yield x' _ -> return (Yield x' extra)
                      Error err -> return (Error err)
                Error err -> return (Error err)





