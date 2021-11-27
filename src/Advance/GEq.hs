{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- |

module Advance.GEq where
import Language.Haskell (Token(OVERLAPPABLE))
import Data.Derive.DSL.HSE (Token(INCOHERENT))

class GEq a b where
  geq :: a -> b -> Bool

instance {-# OVERLAPPABLE #-} Real b => GEq Double b where
  geq a b = toRational a == toRational b

instance {-# INCOHERENT #-} Real a => GEq a Double where
  geq a b = toRational a == toRational b
