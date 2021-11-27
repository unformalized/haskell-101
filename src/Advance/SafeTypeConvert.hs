{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |

module Advance.SafeTypeConvert where

newtype Dollar = Dollar Int deriving (Show, Num)
newtype Pound  = Pound  Int deriving (Show, Num)

type family F a

type instance F Int = Int
type instance F Dollar = Char

class Bad a where
  bad :: a -> F a

instance Bad Int where
  bad = (+1)

deriving instance Bad Dollar
