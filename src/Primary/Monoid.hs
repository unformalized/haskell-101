module Primary.Monoid () where

-- import Data.Semigroup

-- 单位半群类型类 Monoid
-- 数学上的半群：semigroup，是闭合于一个有结合性质的二元运算之下的集合S构成的代数结构，
-- 对于集合 S，以及基于 S 的二元运算 (+) :: S x S -> S, 若满足结合律，
-- forall x,y,z : S, 有 (s (+) y) (+) z == (x) (+) (y (+) z), 有序对 (S, (+)) 构成半群，若在半群 (S, (+)) 中存在单位元
-- e, 使得 (exist e : S, forall s : S, 有 e (+) s == s (+) e == s), 则 (S, (+), e) 称为单位半群（幺半群），单位元 e 称为幺元

-- 钟数字集合 S {1..12} 与 + 加法构成半群
-- (Z, max, -infinity), (Z, min, infinity) 也是单位半群

newtype All = All { getAll :: Bool }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup All where
  (<>) (All a) (All b) = All (a && b)

instance Monoid All where
  mempty = All True
  mappend (All a) (All b) = All (a && b)


-- (f :: a -> a, (.), id) 也是单位半群

-- Endomorphisms 函数自同态
newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  (<>) (Endo f) (Endo g) = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id
  mappend (Endo f) (Endo g) = Endo (f . g)

-- First 半群

newtype First a = First { getFirst :: Maybe a }

instance Semigroup (First a) where
  (<>) (First Nothing)    r = r
  (<>) l@(First (Just _)) _ = l

instance Monoid (First a) where
  mempty = First Nothing
  mappend l@(First (Just _)) _ = l
  mappend (First Nothing) r    = r

-- Last 半群

newtype Last a = Last { getLast :: Maybe a }

instance Semigroup (Last a) where
  (<>) l (Last Nothing)    = l
  (<>) _ r@(Last (Just _)) = r

instance Monoid (Last a) where
  mempty = Last Nothing
  mappend _ r@(Last (Just _)) = r
  mappend l (Last Nothing)    = l
