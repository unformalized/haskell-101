{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Primary.MultiWayIf () where

import Data.Sequence

-- 多条件分支表达式 扩展：MultiWayIf
foo :: (Ord a1, Ord a2, Num a1, Num a2) => a1 -> a2 -> Bool
foo a1 a2 = if | a1 > 10 -> if | a1 < 10 && a2 > 50 -> True
                               | a1 >=10 && a2 < 30 -> False
               | a1 < 10 -> True
               | otherwise  -> False


data Shape = Triangle Int Int Int | Circle Int

isValidShape :: Shape -> Bool
isValidShape s | Circle r <- s, r > 0 = True
isValidShape s | Triangle a b c <- s,
                 a > 0 && b > 0 && c > 0,
                 a + b > c && a + c > b && b + c > a
                 = True
isValidShape _ = False


-- 模式匹配守卫表达式

isValidShape' :: Shape -> Bool
isValidShape' (Circle r) | r > 0 = True
isValidShape' (Triangle a b c) | a > 0 && b > 0 && c > 0 = True
isValidShape' _ = False


foo1 :: Maybe Bool -> Shape -> Bool
foo1 m s | Just x <- m, Circle r <- s, r > 0 = x && True
foo1 m s | Just x <- m, Triangle a b c <- s,
           a > 0 && b > 0 && c > 0,
           a + b > c && a + c > b && b + c > a
           = True
foo1 _ _ = False


-- 观察模式表达式 ViewPattern

one2ten :: Seq Int
one2ten = fromList [1..4]

match :: Seq Int -> Seq Int -> (Int, Seq Int)
match s1 s2 =
  case viewl s1 of
    EmptyL ->
      case viewr s2 of
        EmptyR -> (0, s2)
        xs :> x -> (x, xs)
    a :< as ->
      case viewr s2 of
        EmptyR -> (a, as)
        xs :> x -> (a + x, xs >< as)

match' :: Seq Int -> Seq Int -> (Int, Seq Int)
match' (viewl -> EmptyL) s2@(viewr -> EmptyR) = (0, s2)
match' (viewl -> EmptyL) (viewr -> xs :> x)   = (x, xs)
match' (viewl -> a :< as) (viewr -> EmptyR)   = (a, as)
match' (viewl -> a :< as) (viewr -> xs :> x)  = (a + x, xs >< as)

-- 模式别名

data Exp = Val Int | Exp String [Exp]

pattern Add :: Exp -> Exp -> Exp
pattern Add t1 t2 = Exp "+" [t1, t2]

pattern Sub :: Exp -> Exp -> Exp
pattern Sub t1 t2 = Exp "-" [t1, t2]


eval :: Exp -> Int
eval (Val n) = n
eval (Add t1 t2) = eval t1 + eval t2
eval (Sub t1 t2) = eval t1 - eval t2





