{-# LANGUAGE LambdaCase #-}
module Intermediate.CalcByStack where

import Control.Monad.State ( State, put, state, evalState )
import Data.Char ( isDigit )
import System.Environment ( getArgs )
import System.IO


-- 基于栈的计算器
-- 用 State Monad 存储 数栈和符号栈
-- 遇到数字时直接压入数栈中
-- 遇到一元符号时压入符号栈中，遇到二元符号时进行符号优先级和结合性判断
-- 若栈顶的运算符为你一元前置运算符，且优先级大于输入的运算符，则先取出数栈栈顶与符号栈栈顶元素
-- 进行运算再压入数栈中，再将运算符压入符号栈中
-- 若栈顶为二元运算符，若压入运算符优先级大于栈顶运算符则直接压入，否则取出数栈最上2层和符号栈
-- 栈顶元素进行运算压入数栈中，在压入符号栈

-- 数栈元素，Val 小数，Const 数学常量
data Lit = Val Float | Const String | Empty deriving (Show, Eq)

data Op = Posi | Nega | Plus | Minu | Mult | Divi | Power
        | Log | Ln | Sin | Cos | Sqrt | LPar | RPar | OpBottom
        deriving (Eq, Show)

--            一元     二元     括号    栈底
data Order = Unary | Binary | Null | Bottom

nary :: Op -> Order
nary =
  \case
    Plus -> Binary
    Minu -> Binary
    Mult -> Binary
    Divi -> Binary
    Power -> Binary
    Posi -> Unary
    Nega -> Unary
    Log -> Unary
    Ln -> Unary
    Sin -> Unary
    Cos -> Unary
    Sqrt -> Unary
    OpBottom -> Bottom
    _ -> Null

priority :: Op -> Int
priority =
  \case
    Plus -> 1
    Minu -> 1
    Mult -> 2
    Divi -> 2
    Log -> 3
    Sin -> 3
    Cos -> 3
    Posi -> 4
    Nega -> 4
    Sqrt -> 4
    Power -> 5
    _ -> 0

type LitOp = Either Lit Op
type Stack = ([LitOp], [LitOp])

lv :: Float -> LitOp
lv = Left . Val

evaluate :: Op -> LitOp -> LitOp -> State Stack ()
evaluate op (Left (Val f1)) (Left (Val f2)) =
  case op of
    Plus -> push $ lv (f1 + f2)
    Minu -> push $ lv (f1 - f2)
    Mult -> push $ lv (f1 * f2)
    Divi -> push $ lv (f1 / f2)
    Power -> push $ lv (f2 ** f2)
evaluate op (Left (Val f1)) (Left Empty) =
  case op of
    Posi -> push $ lv f1
    Nega -> push $ lv (-f1)
    Log -> push $ lv (logBase 2 f1)
    Ln -> push $ lv (log f1)
    Sin -> push $ lv (sin f1)
    Cos -> push $ lv (cos f1)
    Sqrt -> push $ lv (sqrt f1)

pop0, pop1 :: State Stack LitOp
pop0 =
  state $ \(ls, rs) ->
    case ls of
      [] -> error "Number stack underflow"
      (h:hs) -> (h, (hs, rs))

pop1 =
  state $ \(ls, rs) ->
    case rs of
      [] -> error "Operator stack underflow"
      (h:hs) -> (h, (ls, hs))


push :: LitOp -> State Stack ()
push (Left (Const "pi")) = push $ lv 3.14
push (Left (Const "e")) = push $ lv 2.71
push (Left (Const c)) = error $ "Unkown Constant" ++ c
push l@(Left a) = state $ \(ls, rs) -> ((), (l : ls, rs))
push r@(Right a)= state $ \(ls, rs) -> ((), (ls, r : rs))

-- 入栈


pushIn :: LitOp -> State Stack ()
pushIn l@(Left num) = push l
pushIn p@(Right LPar) = push p
-- 若是入栈的为右括号，则括号内部的表达式可以被计算了
-- 依次从符号栈中取出符号进行运算，直到出栈的为括号（必定为左括号），则直接 return ()
pushIn p@(Right RPar) = do
  litop <- pop1
  case litop of
    Right top ->
      case nary top of
        Null -> return ()
        Unary -> do
          f1 <- pop0
          evaluate top f1 (Left Empty)
          pushIn p
        Binary -> do
          f1 <- pop0
          f2 <- pop0
          evaluate top f2 f1
          pushIn p
        Bottom -> error "Excepted Left Bracket\n"
    Left _ -> error "pop1 return Num"
pushIn o@(Right op) = do
  case nary op of
    -- 一元运算符直接入栈
    Unary -> push o
    -- 二元运算符与栈顶运算符比较
    Binary -> do
      litop <- pop1
      case litop of
        Right topOp ->
          case nary topOp of
            Unary -> do
              if priority topOp > priority op
              then do
                -- 栈顶元素优先级高于入栈运算符优先级，则先进行计算
                f1 <- pop0
                evaluate topOp f1 (Left Empty)
                pushIn o
              else do
                -- 否则直接入栈
                push (Right topOp)
                push o
            Binary -> do
              case op of
                -- 幂运算特殊处理，直接入栈
                Power -> do
                  push (Right topOp)
                  push o
                -- 其他二元运算，则优先级判断，若是入栈运算符高，则直接入栈，栈顶运算符 >= 入栈运算符，则先进行计算
                _ -> do
                  if priority topOp >= priority op
                    then do
                      f1 <- pop0
                      f2 <- pop0
                      evaluate topOp f2 f1
                      pushIn (Right op)
                    else do
                      push (Right topOp)
                      push o
            _ -> do
              push (Right topOp)
              push o


-- 对词法单元列表进行计算


calc :: [LitOp] -> State Stack LitOp
calc [] = do
  -- 空列表时说明开始进行计算
  op1 <- pop1
  case op1 of
    Right op ->
      case nary op of
        Bottom -> pop0
        Unary -> do
          f1 <- pop0
          evaluate op f1 (Left Empty)
          calc []
        Binary -> do
          f1 <- pop0
          f2 <- pop0
          evaluate op f1 f2
          calc []
        Null -> error "Excepted right bracket"
    _ -> error "pop1: Excepted left num"
calc (t:ts) = do
  pushIn t
  calc ts

-- 初始化
inits :: ([LitOp], [LitOp])
inits = ([], [Right OpBottom])

-- 接下来是完成另一个步骤将 string 转换为 [LitOp]
-- 计算表达式的结构有一定规则，比如乘号不会出现在表达式的开头，数的后边为右括号或者是二元运算符，否则为表达式的末尾。
-- 使用上下文无关法 (context free grammer) 来描述语言。在分析语法时，不必考虑结合性与优先级的问题（由栈的计算进行处理）
-- 先规定术语与符号：表达式（Expression），一元前置运算符开头表达式（UnaryExpression），二元运算符开头的表达式（BinaryExpression），数（Number）
-- 一元运算符 (-) ，二元运算符 (+) , 或空字符串 
-- 算术表达式的语法
data Expression = UnaryE Expression | Expression BinaryExpression | Number BinaryExpression
data BinaryExpression = BianryE Expression | NIL

-- 前缀部分扫描
scanExp :: String -> [LitOp]
scanExp [] = error "Execpted an expression"
scanExp (' ' : ts) = scanExp ts
-- 通过前缀扫描一元运算符
scanExp ('l':'o':'g':ts) = Right Log : scanExp ts
scanExp ('s':'i':'n':ts) = Right Sin : scanExp ts
scanExp ('c':'o':'s':ts) = Right Cos : scanExp ts
scanExp ('s':'q':'r':'t':ts) = Right Sqrt : scanExp ts
scanExp ('+':ts) = Right Posi : scanExp ts
scanExp ('-':ts) = Right Nega : scanExp ts
scanExp ('(':ts) = Right LPar : scanExp ts
-- 若没有一元运算符则是以数字开头
scanExp ts = scanNum ts

-- 一个数学表达式的开头若不是一元运算符，也不是左括号，则必为一个数值，所以有
scanNum :: String -> [LitOp]
scanNum ('e':ts) = Left (Const "e") : scanBin ts
scanNum ('p':'i':ts) = Left (Const "pi") : scanBin ts
scanNum xs
  | null num = error "Unexecpted a number or constant"
  | otherwise = case rest of
    ('.':r) ->
      let (float, r') = span isDigit r
      in Left (Val (read (num ++ "." ++ float) :: Float)) : scanBin r'
    r -> Left (Val (read num :: Float)) : scanBin r
  where
    (num, rest) = span isDigit xs

-- 扫描二元运算符
scanBin :: String -> [LitOp]
scanBin [] = []
scanBin (' ':ts) = scanBin ts
scanBin ('+':ts) = Right Plus : scanExp ts
scanBin ('-':ts) = Right Minu : scanExp ts
scanBin ('*':ts) = Right Mult : scanExp ts
scanBin ('/':ts) = Right Divi : scanExp ts
scanBin ('^':ts) = Right Power : scanExp ts
scanBin (')':ts) = Right RPar : scanBin ts
scanBin _ = error "Execpted an infix binary operator"

cal :: String -> LitOp
cal exp = (evalState . calc . scanExp) exp inits

num :: LitOp -> Float
num (Left (Val a)) = a
num _ = error "input error"

calculate :: String -> Float
calculate = num . cal

calcStart :: IO ()
calcStart = do
  expr <- getLine
  print $ calculate expr



