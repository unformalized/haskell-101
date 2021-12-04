{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

module SYB.SYB where

import Data.Typeable
import GHC.Generics
import Text.PrettyPrint.GenericPretty
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (fromMaybe)

newtype Company = Company { departments :: [Department] } deriving (Show, Generic, Out, Typeable)

data Department = D
  { departmentName :: String
  , manager :: Person
  , workers :: [Person]
  } deriving (Show, Generic, Out, Typeable)

data Person = P
  { personName :: Name
  , gender :: Gender
  , age :: Age
  } deriving (Show, Generic, Out, Typeable)

data Name = N
  { familyName :: String
  , givenName :: String
  } deriving (Show, Generic, Out, Typeable)

data Gender = Male | Female deriving (Show, Generic, Out, Typeable)

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
addAgeP n p = p { age = age p + n }

addAgeD :: Int -> Department -> Department
addAgeD n d@(D _ m ws) =
  d { manager = addAgeP n m, workers = map (addAgeP n) ws }

addAgeC :: Int -> Company -> Company
addAgeC n c@(Company ds) = c { departments = map (addAgeD n) ds }

addAge' :: Typeable a => Int -> a -> a
addAge' n a = case cast a of
  Nothing -> a
  Just p -> unsafeCoerce $ p { age = age p + n }

mkT :: (Typeable a, Typeable b) => (a -> a) -> (b -> b)
mkT f = fromMaybe id (cast f)

addAge :: Typeable b => Int -> b -> b
addAge n = mkT (addAgeP n)

class Typeable a => Data a where
  gmapT :: (forall b. Data b => b -> b) -> a -> a
  gmapQ :: (forall b. Data b => b -> b) -> a -> [r]



