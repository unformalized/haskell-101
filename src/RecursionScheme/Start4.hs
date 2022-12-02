-- /
{-# LANGUAGE DeriveFunctor #-}

module RecursionScheme.Start4 () where

import Control.Arrow ((&&&), (>>>))
import Control.Monad.Identity (Identity (Identity))
import RecursionScheme.Start2 (Term (..))

data Attr f a = Attr
  { attribute :: a,
    hole :: f (Attr f a)
  }

-- 对比于 Algebra :: f a = f a -> a，CVAlgebra 存储了更丰富的信息
type CVAlgebra f a = f (Attr f a) -> a

histo :: Functor f => CVAlgebra f a -> Term f -> a
-- histo h = out >>> fmap worker >>> h
histo h = worker >>> attribute
  where
    -- worker :: Term f -> Attr f a
    -- 如何获取 f (Attr f a)，可以通过 fmap worker (f (Term f)) 得到
    -- worker t = Attr (histo h t) (fmap worker (out t))
    -- 上面的 worker 函数，Attr a b 的两个属性都是分别进行递归求值，并没有共享到状态
    worker = out >>> fmap worker >>> (h &&& id) >>> mkAttr
    mkAttr (a, b) = Attr a b

-- 实现硬币找零问题

type Cent = Int

coins :: [Cent]
coins = [50, 25, 10, 5, 1]

data Nat a = Zero | Next a deriving (Functor, Eq, Show)

expand :: Int -> Term Nat
expand 0 = In Zero
expand n = In (Next (expand (n - 1)))

compress :: Nat (Attr Nat a) -> Int
compress Zero = 0
compress (Next (Attr _ x)) = 1 + compress x

-- 接受一个数额，返回找零的方法数
change :: Cent -> Int
change amt = histo go (expand amt)
  where
    go :: Nat (Attr Nat Int) -> Int
    go Zero = 1
    go curr@(Next attr) =
      let given = compress curr
          validCoins = filter (<= given) coins
       in sum (map (lookup attr) validCoins)
    lookup :: Attr Nat a -> Int -> a
    lookup cache 1 = attribute cache
    lookup cache n = lookup inner (n - 1)
      where
        (Next inner) = hole cache
