{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
-- |

module Generic.GenericShow where

import Data.Typeable
import GHC.Generics

class GShow (a :: * -> *) where
  shows1 :: Bool -> a x -> ShowS

instance GShow V1 where
  shows1 _ _ = error "cannot shows1 Void type"

instance GShow U1 where
  shows1 _ U1 = id

instance (GShow a, GShow b) => GShow (a :+: b) where
  shows1 b (L1 a) = shows1 b a
  shows1 b (R1 a) = shows1 b a

instance (GShow a, GShow b) => GShow (a :*: b) where
  shows1 b (x :*: y) = shows1 b x . shows1 b y
