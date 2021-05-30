{-# LANGUAGE ExistentialQuantification #-}

module Primary.Existential where

data Showy = forall a. (Show a) => Showy a

instance Show Showy where
  show (Showy a) = show a

