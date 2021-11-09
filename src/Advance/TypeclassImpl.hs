module Advance.TypeclassImpl where

class EqSelf a where
  equalSelf :: a -> a -> Bool

newtype EqSelf1 a = MKEq { eqSelf1 :: a -> a -> Bool }

instance EqSelf Bool where
  equalSelf True True   = True
  equalSelf False False = True
  equalSelf _ _         = False

boolEq :: EqSelf1 Bool
boolEq = MKEq eqBool
  where
    eqBool True True   = True
    eqBool False False = True
    eqBool _ _         = False


equalSelf1 :: EqSelf1 a -> a -> a -> Bool
equalSelf1 dict a b = (eqSelf1 dict) a b

