{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module Generic.Generic3 where
import GHC.Generics (Fixity(Prefix))

type role K1 phantom representational phantom
newtype K1 i c p = K1 { unK1 :: c }

data R
data P

type Rec0 = K1 R
type Par0 = K1 P

type role M1 phantom phantom representational nominal
newtype M1 i c (f :: * -> *) p = M1 { unM1 :: f p }

data D -- 数据类型标记
data C -- 数据构造器标记
data S -- 访问器标记

type D1 = M1 D -- 说明标记的是数据类型的信息
type C1 = M1 C -- 说明标记的是数据构造器标记的信息
type S1 = M1 S -- 说明标记的是访问器标记的信息

-- 定义基本代数类型

type role V1 phantom
data V1 p

type role U1 phantom
data U1 p = U1

infixr 5 :+:
data (:+:) f g p = L1 (f p) | R1 (g p)

infixr 6 :*:
data (:*:) f g p = f p :*: g p

infixr 7 :.:
newtype (:.:) f g p = Comp1 { unComp1 :: f (g p) }

newtype Par1 p = Par1 { unPar1 :: p }
newtype Rec1 f p = Rec1 { unRec1 :: f p }

class DataType d where
  datatypeName :: t d (f :: * -> *) a -> String -- 类型名称
  moduleName ::  t d (f :: * -> *) a -> String -- 类型声明所在模块
  isNewtype :: t d (f :: * -> *) a -> Bool
  isNewtype _ = False

class Selector s where
  selName :: t s (f :: * -> *) a -> String

class Constructor c where
  conName :: t c (f :: * -> *) a -> String -- 构造器名称
  conFixity :: t c (f :: * -> *) a -> Fixity -- 构造器的结合性
  conFixity _ = Prefix
  consIsRecord :: t c (f :: * -> *) a -> Bool -- 是否有访问器
  consIsRecord _ = False

