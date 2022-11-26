{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
module RecursionScheme.Start2
  ( ExprF (..),
    Term (..),
    Algebra,
    Coalgebra,
    cata,
    ana,
    bottomUp,
    topDown,
  )
where

import Control.Arrow ((<<<), (>>>))
import RecursionScheme.Start1 (Lit (..))
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P

data ExprF a
  = IdentF {name :: String}
  | IndexF {target :: a, idx :: a}
  | UnaryF {op :: String, target :: a}
  | BinaryF {lhs :: a, op :: String, rhs :: a}
  | CallF {func :: a, args :: [a]}
  | ParenF {target :: a}
  | LiteralF {intVal :: Lit}
  deriving (Show, Eq, Functor)

newtype Term f = In {out :: f (Term f)}

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a

ten, add, call :: Term ExprF
ten = In (LiteralF {intVal = IntLit 10})
add = In (IdentF {name = "add"})
call = In (CallF {func = add, args = [ten, ten]})

mystery :: Functor f => Algebra f a -> Term f -> a
mystery fn = out >>> fmap (mystery fn) >>> fn

cata :: Functor f => Algebra f a -> Term f -> a
cata fn = out >>> fmap (cata fn) >>> fn

ana :: Functor f => Coalgebra f a -> a -> Term f
ana f = In <<< fmap (ana f) <<< f

countNodes :: Algebra ExprF Int
countNodes (UnaryF _ arg) = arg + 1
countNodes (BinaryF left _ right) = left + right + 1
countNodes (CallF fn args) = fn + sum args + 1
countNodes (IndexF it idx) = it + idx + 1
countNodes (ParenF arg) = arg + 1
countNodes (LiteralF _) = 1
countNodes (IdentF _) = 1

printLit :: Lit -> Doc
printLit (String s) = P.text s
printLit (IntLit i) = P.int i
printLit (Ident id) = P.text id

prettyPrint :: Algebra ExprF Doc
prettyPrint (LiteralF i) = printLit i
prettyPrint (IdentF s) = P.text s
prettyPrint (CallF f as) = f <> P.parens (P.cat (P.punctuate ", " as))
prettyPrint (IndexF it idx) = it <> P.brackets idx
prettyPrint (UnaryF op it) = P.text op <> it
prettyPrint (BinaryF l op r) = l <> P.text op <> r
prettyPrint (ParenF exp) = P.parens exp

bottomUp, topDown :: Functor f => (Term f -> Term f) -> Term f -> Term f
bottomUp f = cata (In >>> f)
topDown f = ana (out <<< f)
