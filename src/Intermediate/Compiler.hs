module Intermediate.Compiler where

import Data.List
import Control.Monad.Writer
import Control.Monad.State

type Name = Char
type Label = Int
type Code = [Inst]
type Stack = [Int]

-- compiler
-- 1：保持一个状态，可以不断生成不同的标签值进行程序跳转
-- 2：记录生成的机器指令
type WT a = WriterT Code (State Int) a

data Exp =  Val Int | Var Name | App Op Exp Exp deriving Show
data Op = Add | Sub | Mul | Div deriving (Show, Eq)
data Inst = PUSH Int 
          | PUSHV Name
          | POP Name 
          | DO Op 
          | JUMP Label
          | JUMPZ Label
          | LABEL Label
          deriving Show

comexp :: Exp -> Code
comexp (Val int) = [PUSH int]
comexp (Var name) = [PUSHV name]
comexp (App op e1 e2) = comexp e1 ++ comexp e2 ++ [DO op]

data Prog = Assign Name Exp
          | If Exp Prog Prog
          | While Exp Prog
          | Seqn [Prog]
          deriving Show

factorial :: Int -> Prog
factorial n = Seqn [Assign 'A' (Val 1),
                    Assign 'B' (Val n),
                    While (Var 'B') (
                      Seqn [Assign 'A' (App Mul (Var 'A') (Var 'B')),
                            Assign 'B' (App Sub (Var 'B') (Val 1))]
                    )]

-- fresh 生成一个 label，在 do 中 s 会一直增加
fresh :: WT Int
fresh = WriterT $ state (\s -> ((s, mempty), s + 1))

mlabel :: Prog -> WriterT Code (State Int) ()
-- tell 写入指令
-- 
mlabel (Assign name expr) = do
  tell $ comexp expr
  tell [POP name]
mlabel (If expr prog1 prog2) = do
  -- 生成 if 的 then 和 else 标签
  n <- fresh
  m <- fresh
  tell $ comexp expr
  -- 若栈顶为 0, 即进入 else 标签
  tell [ JUMPZ n ]
  mlabel prog1
  --  若进入 then 标签，则需要跳过 else 标签，来到 m
  tell [ JUMP m ]
  -- 生成标签 n
  tell [ LABEL n ]
  mlabel prog2
  tell [ LABEL m ]
mlabel (While expr prog) = do
  n <- fresh
  m <- fresh
  -- 生成标签 n
  tell [ LABEL n ]
  tell $ comexp expr
  tell [ JUMPZ m ]
  mlabel prog
  tell [ JUMP n ]
  tell [ LABEL m ]
mlabel (Seqn []) = do tell []
mlabel (Seqn (c:cs)) = do mlabel c
                          mlabel (Seqn cs)


-- 编译程序为机器代码

comp :: Prog -> Code
comp prog = snd $ fst $ (runState $ runWriterT $ mlabel prog) 0



