{-# LANGUAGE GADTs #-}

module Primary.LogicCalculator where

data Formula ts where
  Body   :: Term Bool -> Formula ()
  Forall :: Show a => [a] -> (Term a -> Formula as) -> Formula (a, as)
  Exist  :: Show a => [a] -> (Term a -> Formula as) -> Formula (a, as)

data Term t where
  Con   :: a -> Term a
  (:&:) :: Term Bool -> Term Bool -> Term Bool
  (:|:) :: Term Bool -> Term Bool -> Term Bool
  (:<:) :: Term Int -> Term Int -> Term Bool
  (:=:) :: Term Int -> Term Int -> Term Bool
  (:+:) :: Term Int -> Term Int -> Term Int
  (:-:) :: Term Int -> Term Int -> Term Int
  Name  :: String -> Term t

ex1 :: Formula ()
ex1 = Body (Con True)

ex2 :: Formula (Int, ())
ex2 = Forall [1..10] $ \n -> Body $ n :<: (n :+: Con 1)

ex3 :: Formula (Bool, (Int, ()))
ex3 =
  Forall [False, True]  $ \p ->
    Forall [0..2] $ \n -> Body $ p :|: (Con 0 :<: n)

ex4 :: Formula (Int, (Bool, ()))
ex4 = Forall [1,2] $ \n -> Exist [False, True] $ \b -> Body $ b :|: (n :<: Con 2)

evalFormula :: Term t -> t
evalFormula (Con v) = v
evalFormula (p :&: q) = evalFormula p && evalFormula q
evalFormula (p :|: q) = evalFormula p || evalFormula q
evalFormula (p :<: q) = evalFormula p <  evalFormula q
evalFormula (p :+: q) = evalFormula p +  evalFormula q
evalFormula (p :=: q) = evalFormula p == evalFormula q
evalFormula (p :-: q) = evalFormula p -  evalFormula q
evalFormula (Name _)  = error "cannot evalFormula Name"

satisfiable :: Formula ts -> Bool
satisfiable (Body body) = evalFormula body
satisfiable (Forall xs as) = and [satisfiable (as (Con y)) | y <- xs]
satisfiable (Exist  xs as) = or  [satisfiable (as (Con y)) | y <- xs]

solutions :: Formula ts -> [ts]
solutions (Body body) = [()]
solutions (Forall [] as) = []
solutions f@(Forall (x:xs) as)
  | satisfiable f =
    [(x,fs) | fs <- solutions (as (Con x))] ++ solutions (Forall xs as)
  | otherwise = []
solutions (Exist [] as) = []
solutions (Exist (x:xs) as) =
    [(x, fs) | satisfiable (as (Con x)), fs <- solutions (as (Con x))] ++ solutions (Exist xs as)












