-- not export anything
module Base.BoolFunction () where

import Prelude hiding ((==), (/=), or, and, xor, (&&), (||), not)

(==) :: Bool -> Bool -> Bool
(==) True True   = True
(==) False False = True
(==) _ _         = False

not :: Bool -> Bool
not True = False
not _    = True

xor, and, or :: Bool -> Bool -> Bool
xor b1 b2 = not (b1 == b2)

and True b1 = b1
and False _ = False

or False b1 = b1
or True  _  = True


condition :: Bool ->  a -> a -> a
condition True  t f = t
condition False t f = f

infix 4 ==
infix 4 /=
infixl 3 &&
infixl 2 ||

(||) = or
(&&) = and
(/=) = xor

-- halfadder

ha :: Bool -> Bool -> (Bool, Bool)
ha a b = (a /= b, a && b)

-- fulladder

fa :: Bool -> Bool -> Bool -> (Bool, Bool)
fa a b c =
  let (haS1, haC1) = ha a b
  in let (haS2, haC2) = ha haS1 c
     in (haS2, haC1 || haC2)

-- nand nor
nand, nor :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True

nor False False = True
nor _ _ = False

not1, not2 :: Bool -> Bool
not1 b = nand b b
not2 b = nor b b

and1, and2 :: Bool -> Bool -> Bool
and1 a b = not1 (nand a b)
and2 a b = nor (not2 a) (not2 b)

or1, or2 :: Bool -> Bool -> Bool
or1 a b = nand (not1 a) (not1 b)
or2 a b = not2 $ nor a b


