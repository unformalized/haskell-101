module Advance.UseOfCoerce where

import Data.Coerce

newtype MyIdentity a = MyIdentity { runMyId :: a }

instance Functor MyIdentity where
  fmap = coerce

instance Applicative MyIdentity where
  pure = MyIdentity
  (<*>) = coerce

