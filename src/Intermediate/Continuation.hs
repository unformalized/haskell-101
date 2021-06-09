{-# LANGUAGE DeriveFunctor #-}
module Intermediate.Continuation where

factCps :: (Eq a, Num a) => a -> (a -> t) -> t
factCps 0 k = k 1
factCps n k = factCps (n - 1) (\x -> k (n * x))

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibCps :: Int -> (Int -> Int) -> Int
fibCps 0 k = k 1
fibCps 1 k = k 1
fibCps n k = fibCps (n - 1) (\s -> fibCps (n - 2) (\s' -> k (s' + s)))


-- 实现 Continuation Monad
-- Cont 与 State 的理解方式是一样的
-- Cont 相当于存储了一个值，return 和 pure 函数，构造得到的 Cont 需要一个后续函数消费这个值,
-- runCont Cont f, f 为消费函数
-- 通过 Cont $ \f -> _todo1，f 就是后续的消费函数，_todo1 你必须构造出一个给 f 消费的值，产生最终的结果 r
-- 在 Monad 的世界里，并不只有人手动的调用 runCont Cont f 对 Cont 中生产的值进行消费
-- 使用 >>= 进行组合时，ContA 和 f 组合出一个产生 b 类型值 Cont，如何产生 b 呢？先对 ContA 进行消费，得到 a
-- runCont ContA \a -> _todo2, 然后让 f a 构造一个 ContB 让新的 Cont 的消费函数 k 进行消费 
-- (>>=) contA f = Cont $ \k -> runCont contA (\a -> runCont (f a) k)
-- 再组合一个 g :: b -> Cont r c
-- z :: c -> r
-- ((>>=) contA f) >>= g = Cont $ \z -> runCont ((>>=) contA f) (\b -> runCont (g b) z)
-- State 通过 >>= 组合一个个状态转变的过程，最终传入一个初始状态，得到一个结果状态 runState StateA initialState
-- Cont 通过 >>= 组合一个值与一个消费函数，得到一个新值，再组合一个消费函数得到一个新值，这一切都在 Cont 内部
-- 所以 Cont 内部有值，需要传入一个消费函数


newtype Cont r a = Cont { runCont :: (a -> r) -> r } deriving Functor

instance Applicative (Cont r) where
  pure a = Cont $ \f -> f a
  -- f :: ((a -> b) -> r) -> r
  -- a :: (a -> r) -> r
  -- k :: (b -> r)
  (<*>) contF contA = Cont $ \k -> runCont contF (\f -> runCont contA (k . f))

instance Monad (Cont r) where
  return = pure
  (>>=) contA f = Cont $ \k -> runCont contA (\a -> runCont (f a) k)


factCps2 :: Int -> Cont r Int
factCps2 0 = return 1
factCps2 n = do
  n1 <- factCps2 (n - 1)
  return (n * n1)

factCps2' :: Int -> Cont r Int
factCps2' 0 = return 1
factCps2' n = factCps2' (n - 1) >>= (\n1 -> return (n * n1))


plus1 :: Int -> Cont r Int
plus1 n = return (n + 1)

div10 :: Int -> Cont r Int
div10 n = return (div n 10)


foo :: Int -> Cont r Int
foo n = factCps2 n >>= (\r1 -> div10 r1 >>= (\r2 -> plus1 r1))


-- callCC

class Monad m => MonadCont m where
  callCC :: ((a -> m b) -> m a) -> m a

instance MonadCont (Cont r) where
  -- callCC 本质是创造一个 Cont
  -- 它的消费函数 h
  callCC f = Cont $ \h -> runCont (f (\a -> Cont $ \_ -> h a)) h


factCps4 :: Int -> Cont Int Int
factCps4 0 = return 1
factCps4 n = do
  n1 <- factCps4 (n - 1)
  callCC $ \k ->
    let r = n * n1
    in if r > 10000
        then k 0
        else return r

factCps4' :: Int -> Cont Int Int
factCps4' 0 = return 1
factCps4' n =
  factCps4' (n - 1) >>=
    (\n1 -> callCC $ \k ->
      let r = n * n1
      in
        if r > 10000
          then k 0
          else return r)

--
fibs2 :: Int -> Cont r Int
fibs2 0 = return 1
fibs2 1 = return 1
fibs2 n = do
  -- 可以不用管 k 函数
  n1 <- callCC $ \k -> fibs2 (n - 1)
  n2 <- callCC $ \k -> fibs2 (n - 2)
  return (n1 + n2)


factCps5 :: Int -> Cont Int Int
factCps5 n = do
  (goto, acc, num) <-
    callCC $ \k ->
      let f x y = k (f, x, y)
      in return (f, 1, n)
  if num == 1
    then return acc
    else goto (acc * num) (num - 1)


factCps5' :: Int -> Cont Int Int
factCps5' n =
  callCC (\k ->
    let f x y = k (f, x, y)
    in return (f, 1, n)) >>= \(goto, acc, num) -> if num == 1 then return acc else goto (acc * num) (num - 1)

-- 展开 callCC
factCps5'' n =
  Cont (\h -> runCont ((\k -> let f x y = k (f, x, y)
                             in return (f, 1, n)) (\a -> Cont $ \_ -> h a)) h)
  >>= \(goto, acc, num) -> if num == 1
                            then return acc
                            else goto (acc * num) (num - 1)

-- 展开 >>=

factCps5''' n =
  Cont (\c ->
          runCont (Cont (\h -> runCont ((\k -> let f x y = k (f, x, y)
                             in return (f, 1, n)) (\a -> Cont $ \_ -> h a)) h))
                  (\a -> runCont ((\(goto, acc, num) ->
                                    if num == 1
                                      then return acc
                                      else goto (acc * num) (num - 1)) a) c)
       )

-- (\a -> Cont $ \_ -> h a)

-- 去掉 Cont 和 runCont
factCps6 :: Int -> (Int -> Int) -> Int
factCps6 n c =
    (\h -> (let f x y = \_ -> h (f, x, y)
            in \k -> k (f, 1, n)) h)
    (\a -> (\(goto, acc, num) ->
             if num == 1
             then \k -> k acc
             else goto (acc * num) (num - 1)) a c)
