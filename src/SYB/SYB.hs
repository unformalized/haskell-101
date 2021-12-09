{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeSynonymInstances #-}

module SYB.SYB where

import Data.Data
import Data.Maybe (fromMaybe)
import Data.Typeable
import GHC.Generics
import Text.PrettyPrint.GenericPretty
import Unsafe.Coerce (unsafeCoerce)

newtype Company = Company {departments :: [Department]} deriving (Show, Generic, Out, Typeable, Data)

data Department = D
  { departmentName :: String,
    manager :: Person,
    workers :: [Person]
  }
  deriving (Show, Generic, Out, Typeable, Data)

data Person = P
  { personName :: Name,
    gender :: Gender,
    age :: Age
  }
  deriving (Show, Generic, Out, Typeable, Data)

data Name = N
  { familyName :: String,
    givenName :: String
  }
  deriving (Show, Generic, Out, Typeable, Data)

data Gender = Male | Female deriving (Show, Generic, Out, Typeable, Data)

type Age = Int

microsoft_research_cambridge :: Company
microsoft_research_cambridge = Company [research_haskell, research_fsharp]

research_haskell :: Department
research_haskell = D "Haskell Group" simon_peyton_jones [simon_marlow, ralf_lammel]

research_fsharp :: Department
research_fsharp = D "F# Group" don_syme [andrew_kennedy]

simon_peyton_jones :: Person
simon_peyton_jones = P (N "Peyton_jones" "Simon") Male 55

simon_marlow :: Person
simon_marlow = P (N "Marlow" "simon") Male 30

ralf_lammel :: Person
ralf_lammel = P (N "Lammel" "Ralf") Male 30

don_syme :: Person
don_syme = P (N "Syme" "Don") Male 40

andrew_kennedy :: Person
andrew_kennedy = P (N "Kennedy" "Andrew") Male 35

addAgeP :: Int -> Person -> Person
addAgeP n p = p {age = age p + n}

addAgeD :: Int -> Department -> Department
addAgeD n d@(D _ m ws) =
  d {manager = addAgeP n m, workers = map (addAgeP n) ws}

addAgeC :: Int -> Company -> Company
addAgeC n c@(Company ds) = c {departments = map (addAgeD n) ds}

addAge' :: Typeable a => Int -> a -> a
addAge' n a = case cast a of
  Nothing -> a
  Just p -> unsafeCoerce $ p {age = age p + n}

mkT :: (Typeable a, Typeable b) => (a -> a) -> (b -> b)
mkT f = fromMaybe id (cast f)

addAge :: Typeable b => Int -> b -> b
addAge n = mkT (addAgeP n)

{-
class Typeable a => Data a where
  gmapT :: (forall b. Data b => b -> b) -> a -> a
  gmapQ :: (forall b. Data b => b -> b) -> a -> [r]
 -}

everywhere :: Data a => (forall b. Data b => b -> b) -> a -> a
everywhere f x = f (gmapT (everywhere f) x)

testAddAgeForMicrosoftResearch :: Company
testAddAgeForMicrosoftResearch = everywhere (addAge 1) microsoft_research_cambridge

-- 信息查询
mkQ ::
  (Typeable a, Typeable b) =>
  r -> -- 默认结果
  (b -> r) -> -- 请求函数结果
  a -> -- 树状数据结果
  r -- 最后结果
mkQ r q a = maybe r q (cast a)

even' :: Int -> Bool
even' = even

falseIsEven :: Maybe Bool
falseIsEven = mkQ Nothing (return . even') "abc"

_10IsEven :: Bool
_10IsEven = mkQ False even' (10 :: Int)

simonMarlowFamilyName :: Maybe String
simonMarlowFamilyName = mkQ Nothing (return . familyName) simon_marlow

allNameSimonMarlow :: [Maybe String]
allNameSimonMarlow = gmapQ (mkQ Nothing (return . familyName)) simon_marlow

everything ::
  Data a =>
  (r -> r -> r) -> -- 合并结果的二元函数
  (forall b. Data b => b -> r) -> -- 请求结果的函数
  a -> -- 树状数据结构
  r -- 合并后的结果
everything k f x = foldl k (f x) (gmapQ (everything k f) x)

getFamilyName :: Person -> [String]
getFamilyName = return . familyName . personName

allFamilyNameResearch :: [String]
allFamilyNameResearch = everything (++) (mkQ [] getFamilyName) microsoft_research_cambridge
