{-# LANGUAGE DeriveFunctor #-}

-- |
module RecursionScheme.Start1 (Lit (..)) where

import Control.Arrow ((>>>))
import Control.Category ((<<<))

data Lit
  = String String
  | IntLit Int
  | Ident String
  deriving (Show, Eq)

data Expr
  = Index Expr Expr
  | Call Expr [Expr]
  | Unary Expr Expr
  | Binary Expr String Expr
  | Paren Expr
  | Literal Lit
  deriving (Show, Eq)

data Stmt
  = Break
  | Continue
  | Empty
  | IfElse Expr [Stmt] [Stmt]
  | Return (Maybe Expr)
  | While Expr [Stmt]
  | Expression Expr
  deriving (Show, Eq)

applyExpr :: (Expr -> Expr) -> Expr -> Expr
applyExpr f (Literal i) = Literal i
applyExpr f (Paren p) = Paren p
applyExpr f (Index e i) = Index (f e) (f i)
applyExpr f (Call e args) = Call (f e) (map f args)
applyExpr f (Binary e1 s e2) = Binary (f e1) s (f e2)
applyExpr f (Unary op e) = Unary (f op) (f e)

-- flatten:: remove the paren of expr
flatten :: Expr -> Expr
flatten (Paren e) = flatten e
flatten x = applyExpr flatten x

-- 这里将构造子中的 Expr 全部替换为参数 a
-- 那么现在就无法自然表达出任意嵌套的 Expr，而需要借助 a 参数来表达
data ExprF a
  = IndexF a
  | IdentF String
  | CallF a [a]
  | UnaryF a [a]
  | BinaryF a String a
  | ParenF a
  | LiteralF Lit
  deriving (Show, Eq, Functor)

-- type NestedExpr = Expr (Expr (Expr (...)))
-- 自然而然的就想到了不动点组合子
-- type Y t = t (t (t (t ..)))

newtype Term f = In (f (Term f))

out :: Term f -> f (Term f)
out (In t) = t

-- Term Expr = In (Expr (Term Expr))

topDown, bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn =
  out -- unpack
    >>> fmap (bottomUp fn) -- recursive
    >>> In -- repack
    >>> fn -- apply
topDown fn =
  In
    <<< fmap (topDown fn)
    <<< out
    <<< fn

flattenTerm :: Term ExprF -> Term ExprF
flattenTerm (In (ParenF e)) = e
flattenTerm other = other

flattenF :: Term ExprF -> Term ExprF
flattenF = bottomUp flattenTerm
