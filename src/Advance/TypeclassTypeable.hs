{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Advance.TypeclassTypeable where

import Data.Rank1Dynamic
import Data.Constraint
import Data.Rank1Typeable


deriving instance Typeable Monad
deriving instance Typeable Num

dictMaybe :: Dict (Monad Maybe)
dictMaybe = Dict


-- monadReturn :: Dict (Monad ANY )
-- monadReturn dict = return

-- plus :: Dict (Num ANY) -> ANY -> ANY -> ANY
plus = toDynamic ((\Dict -> (+)) :: Dict (Num ANY) -> ANY -> ANY -> ANY)
dictInt = toDynamic (Dict :: Dict (Num Integer))
dyn5  = toDynamic (5 :: Integer)

applyPlus :: Either TypeError Dynamic
applyPlus = do
    res <- dynApply plus dictInt
    dynApply res dyn5

maybem = typeOf ((return) :: (Int -> Maybe Int))

maybem' = typeOf ((\Dict -> return) :: (Dict (Monad Maybe) -> Int -> Maybe Int))

-- monad = typeOf ((\Dict -> return) :: Dict (Monad ANY) -> ANY1 -> ANY (ANY1 :: *))