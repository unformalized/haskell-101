{-# LANGUAGE GADTs #-}

module Primary.GADTS where

data Exp = ValInt Int
         | ValBool Bool
         | Add Exp Exp
         | Equa Exp Exp
         deriving (Eq, Show)

evalexp :: Exp -> Either Int Bool
evalexp (ValInt a) = Left a
evalexp (ValBool a) = Right a
evalexp (Add e1 e2) =
  case evalexp e1 of
    Left a ->
      case evalexp e2 of
        Left b -> Left (a + b)
evalexp (Equa e1 e2) =
  case evalexp e1 of
    Right a ->
      case evalexp e2 of
        Right b -> Right (a == b)

-- evalexp 中没有匹配 Add (ValInt 1) (ValBool False) 这种表达式，显然是符合要求的

-- 为 Exp 添加更多的参数类型保证上述表达式不能通过编译
-- 虽然加了虚幻类型但还是不能起作用，
-- ValInt' 1 的类型为 Exp' a, a 并没有指明为哪个具体类型

data Exp' a = ValInt' Int
            | ValBool' Bool
            | Add' (Exp' Int) (Exp' Int)
            | Equa' (Exp' a) (Exp' a)
            deriving (Show, Eq)

-- 就算声明了 Add' (Exp' Int) (Exp' Int) 还是没用，ValBool' 属于 Exp' a 就通过了 Add' 的类型检查
exp1 = Add' (ValInt' 1) (ValBool' False)

-- 所以需要更加强大的功能来指明 ValInt 5， ValBool False 属于什么类型


-- 其实 GADT 就是我们对 Haskell 系统对 ADT 的类型推导不满意，自己来写更加具体的类型
data ExpT a where
  ValIntT :: (Num a) => a -> ExpT a
  ValBoolT :: Bool -> ExpT Bool
  AddT :: (Num a) => ExpT a -> ExpT a -> ExpT a
  EquaT :: ExpT Bool -> ExpT Bool -> ExpT Bool


evalT :: ExpT a -> a
evalT (ValIntT a) = a
evalT (ValBoolT b) = b
evalT (AddT a b) = evalT a + evalT b
evalT (EquaT b1 b2) = evalT b1 == evalT b2

exp2 = evalT (AddT (ValIntT 1) (ValIntT 2))




