{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Advance.RecordsPrototype where
import GHC.TypeLits
import Data.Typeable

-- t 重载的访问器所在的数据类型, n 访问器的名字
type family FieldType (t :: *) (n :: Symbol) :: *

-- a 更新的类型
type family UpdateType (t :: *) (n :: Symbol) (a :: *) :: *

-- 所有存在名为 n 的访问器类型
class Has r n where
  getField :: Proxy n -> r -> FieldType r n

-- 所有存在名为 n 的访问器且可以被更新为 a 类型的类型
class (Has r n, r ~ UpdateType r n (FieldType r n)) => Update r n a where
  setField :: Proxy n -> r -> a -> UpdateType r n a

class Accessor (p :: * -> * -> *) (r :: *) (n :: Symbol) where
  accessField :: Proxy n ->
          (Has r n => r -> FieldType r n) ->
          (forall a . Update r n a => r -> a -> UpdateType r n a) ->
          p r (FieldType r n)

instance Has r n => Accessor (->) r n where
  accessField _ g _ = g

field :: Accessor p r n => Proxy n -> p r (FieldType r n)
field z = accessField z (getField z) (setField z)

foo :: Accessor p r "foo" => p r (FieldType r "foo")
foo = field (Proxy :: Proxy "foo")


newtype R a = MkR { foo1 :: a -> a }


type instance FieldType (R a) "foo" = a -> a
type instance UpdateType (R a) "foo" (b -> b) = R b

instance Has (R a) "foo" where
  getField _ r = foo1 r

instance (t ~ (b -> b)) => Update (R a) "foo" t where
  setField _ (MkR _) x = MkR x


data U a = MkU { foo2 :: R a, bar1 :: a }

type instance FieldType (U a) "foo" = R a
type instance UpdateType (U a) "foo" (R c) = U c

instance Has (U a) "foo" where
  getField _ = foo2

instance t ~ R a => Update (U a) "foo" t where
  setField _ (MkU _ y) x = MkU x y

overloadTestR :: R Int
overloadTestR = MkR (+1)

overloadTestU :: U Int 
overloadTestU = MkU overloadTestR 100

