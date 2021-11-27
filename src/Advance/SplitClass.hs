-- |
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advance.SplitClass where

import Data.Proxy

class PlusPrint a where
  test :: a -> IO ()

data NumType = Ints | Reals

type family GetNumType (a :: *) :: NumType where
  GetNumType Int = Ints
  GetNumType Integer = Ints
  GetNumType Double = Reals
  GetNumType Float = Reals

class PlusPrint' (flag :: NumType) a where
  test' :: Proxy flag -> a -> IO ()

instance (Integral a, Show a) => PlusPrint' Ints a where
  test' _ a = print $ a + 1

instance (RealFloat a, Show a) => PlusPrint' Reals a where
  test' _ a = print $ a + 2

instance (GetNumType a ~ flag, PlusPrint' flag a) => PlusPrint a where
  test = test' (Proxy :: Proxy flag)
