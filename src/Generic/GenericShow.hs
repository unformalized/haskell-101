{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- |

module Generic.GenericShow where

import Data.Typeable
import GHC.Generics

-- 定义一个辅助类型类， shows1 的参数：第一个布尔值表示如果类型有访问器则输出时加上大括号，
class GShow (a :: * -> *) where
  shows1 :: Bool -> a x -> ShowS

-- 实现基本代数类型
instance GShow V1 where
  shows1 _ _ = error "cannot shows1 Void type"

instance GShow U1 where
  shows1 _ U1 = id

instance (GShow a, GShow b) => GShow (a :+: b) where
  shows1 b (L1 a) = shows1 b a
  shows1 b (R1 a) = shows1 b a

instance (GShow a, GShow b) => GShow (a :*: b) where
  shows1 b (x :*: y) = shows1 b x . shows1 b y

instance (Show0 a) => GShow (K1 i a) where
  shows1 _ (K1 a) = \x -> show0 a ++ x

instance (GShow a) => GShow (D1 b a) where
  shows1 b (M1 a) = shows1 b a

instance (GShow a, Constructor g) => GShow (C1 g a) where
  shows1 _ c@(M1 a) = showString "(" .
                      showString (conName c) .
                      showString " " .
                      -- 
                      wrapRecord (shows1 (conIsRecord c) a) .
                      showString ")"
                    where
                      wrapRecord :: ShowS -> ShowS
                      wrapRecord s | conIsRecord c = showString "{ " . s . showString " }"
                                   | otherwise = s

instance (GShow a, Selector g) => GShow (S1 g a) where
  shows1 b s@(M1 a) | null (selName s) = shows1 b a
                    | otherwise = showString (selName s) . showString " = " . shows1 b a . showChar ' '


class Show0 a where
  show0 :: a -> String
  default show0 :: (Generic a, GShow (Rep a)) => a -> String
  show0 x = show_default x ""

show_default :: (Generic a, GShow (Rep a)) => a -> ShowS
show_default x = shows1 False (from x)

instance Show0 Char where
  show0 = show

instance Show0 Int where
  show0 = show

instance Show0 Bool where
  show0 = show

instance Show a => Show0 [a] where
  show0 = show

data Person = Person { name :: String, age :: Int } deriving (Eq, Generic, Show0)
data Person' = P String Int deriving (Eq, Generic, Show0)
data Nat = Zero | Succ Nat deriving (Eq, Generic, Show0)
data Sum a b = L a | R b deriving (Eq, Generic, Show0)

testShow0Person = show0 $ Person "Song" 24

testShow0Person' = show0 $ P "Song" 24

testShow0Nat = show0 (Succ (Succ Zero))

testShow0Sum = show0 (L (Person "Song" 24) :: Sum Person Bool)
