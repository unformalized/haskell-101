{-# LANGUAGE MonomorphismRestriction #-}

module Advance.MonomorphismRestriction where

import Data.List
import GHC.Exts

x = 1 + 1

monof xs =
  let len = genericLength xs
  in (len, len)


-- testReads = reads "()"
