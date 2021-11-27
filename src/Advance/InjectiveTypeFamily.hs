{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Advance.InjectiveTypeFamily where

-- F 不为单射函数
type family F a
type instance F Int = Char
type instance F Bool = Char

-- G 为单射函数，并且 G a 的结果依赖于 a
type family G a = b | b -> a
type instance G Int = Char 
type instance G Bool = String

foo :: G a -> G a
foo = id


a = foo 'c'
