{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Test.BinaryTest where


import Data.Binary (Binary, encode)
import GHC.Generics (Generic, Generic1)

data List a = Nil | Cons a (List a) deriving (Eq, Show, Generic, Generic1, Binary)

testEncode = encode (Cons True (Cons False Nil))

