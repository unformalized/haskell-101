{-# LANGUAGE StandaloneDeriving #-}

module HaskellInDepth.Chapter2.ReadAndShow where

data Person = Person String (Maybe Int)

homer :: Person
homer = Person "Homer Simpson" (Just 39)

spj :: Person
spj = Person "Simon Peyton Jones" Nothing

-- deriving instance Show Person
deriving instance Read Person
deriving instance Eq Person

instance Show Person where
  show (Person name Nothing) = name
  show (Person name (Just age)) = name ++ "(" ++ show age ++ ")"
