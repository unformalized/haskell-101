module Intermediate.CFG where

import Text.Parsec
import Intermediate.Parsec

-- Context Free Grammer 上下文无关文法
-- 上下文无关文法实际为一个四元元组，用 G 表示，G = (N,T,P,S)
-- N 为非终结符号（non-terminal）的集合，元素一般为大写，T 为终结符号的集合，元素一般为小写，P 为生产式（production rule）。S为起始符号，S 一般是集合 N 中的元素

data Exp = Add Exp Exp | Mul Exp Exp | Val Double deriving (Eq, Show)

eval :: Exp -> Double
eval (Val v) = v
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2


eval' :: Parsec String () Double
eval' = do
  eval <$> parseExp

calculateExp :: String -> IO ()
calculateExp = parseTest eval'

-- Exp ::= Mul Exp'
parseExp :: Parsec String () Exp
parseExp = do
  e1 <- parseMul
  e2 <- parseExp'
  case e2 of
    Nothing -> return e1
    Just e -> return (e e1)

-- Exp' ::= + Mul Exp' | e
parseExp' :: Parsec String () (Maybe (Exp -> Exp))
parseExp' = try (do
  char '+'
  e1 <- parseMul
  e2 <- parseExp'
  case e2 of
    Nothing -> return (Just (`Add` e1))
    Just e -> return (Just (\e' -> e (Add e' e1)))
  ) <|> return Nothing

-- Mul ::= Num Mul'

parseMul :: Parsec String () Exp
parseMul = do
  e1 <- parseNum
  e2 <- parseMul'
  case e2 of
    Nothing -> return e1
    Just e -> return (e e1)


-- Mul' ::= * Num Mul' | e

parseMul' :: Parsec String () (Maybe (Exp -> Exp))
parseMul' = try (do
  char '*'
  e1 <- parseNum
  e2 <- parseMul'
  case e2 of
    Nothing -> return (Just (`Mul` e1))
    Just e -> return (Just (\e' -> e (Mul e' e1)))) <|> return Nothing

-- Num ::= (Exp) | Number

parseNum :: Parsec String () Exp
parseNum = try (
  do
    char '('
    e1 <- parseExp
    char ')'
    return e1) <|>
  (do
    Val <$> float1
  )

testExp :: IO ()
testExp = parseTest parseExp "1.0+2.0+3.0"


calculate' :: String -> Double
calculate' str =
  case runParser parseExp () "" str of
    Right exp -> eval exp
    Left _ -> error "error"


