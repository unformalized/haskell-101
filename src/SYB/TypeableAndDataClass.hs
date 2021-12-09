{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SYB.TypeableAndDataClass where

{- post: https://chrisdone.com/posts/data-typeable/ -}

import Control.Monad.State
import Data.Char (isUpper)
import Data.Data
import Data.Maybe (fromJust)
import Data.Typeable
import System.IO.Unsafe (unsafePerformIO)

-- print the type of something
charStr :: TypeRep
charStr = typeOf 'a'

-- compare the type of two things
compareR1 = typeOf 'a' == typeOf 'b'

-- reifying from generic to concrete

char :: Typeable a => a -> String
char x = case cast x of
  Just (a :: Char) -> show a
  Nothing -> "Unknown"

-- Data class

-- get data type
datatypeJustA :: DataType
datatypeJustA = dataTypeOf (Just 'a')

-- inspecting a data type

maybeConstr :: [Constr]
maybeConstr = dataTypeConstrs datatypeJustA

-- get constr by index

just :: Constr
just = indexConstr datatypeJustA 2 -- Just

-- is algebraic -- albebraic data type

isAlg :: Bool
isAlg = isAlgType datatypeJustA

notAlg :: Bool
notAlg = isAlgType (dataTypeOf 'a')

-- get the constructor of a value

justConstr :: Constr
justConstr = toConstr (Just 'a')

nothingConstr :: Constr
nothingConstr = toConstr (Nothing :: Maybe Char)

-- constr get back the dataRep

maybeDataRep :: ConstrRep
maybeDataRep = constrRep justConstr

-- get fields of constructor

data X = X {bar :: Int, foo :: Char} deriving (Data, Typeable, Show)

xFields :: [String]
xFields = constrFields (toConstr (X 1 'D'))

-- make a real value from its constructor

-- fromConstr :: Data a => Constr -> a

nothingTuple :: Maybe ()
nothingTuple = fromConstr nothingConstr

-- constructor has fields

-- fromConstrB :: forall a. Data a => (forall d. Data d => d) -> Constr -> a

-- error
-- maybe5 = fromConstrB 5 (toConstr (Just 5 :: Maybe Int))

just5 :: Maybe Int
just5 = fromConstrB (fromConstr (toConstr (5 :: Int))) (toConstr (Just 5 :: Maybe Int))

-- more fields

-- fromConstrM :: forall m a. (Monad m, Data a) => (forall d. Data d => m d) -> Constr -> m a

x5Str :: X
x5Str =
  evalState
    ( fromConstrM
        ( do
            i <- get
            modify (+ 1)
            return
              ( case i of
                  0 -> fromConstr (toConstr (5 :: Int))
                  1 -> fromConstr (toConstr 'c')
              )
        )
        (toConstr (X 4 's'))
    )
    0

-- mapping over data structures generically

-- gmapT :: forall a. Data a => (forall b. Data b => b -> b) -> a -> a

x5a :: X
x5a =
  gmapT
    ( \d ->
        case cast d of
          Nothing -> d
          Just x ->
            fromJust (cast (if isUpper x then '!' else x))
    )
    (X 5 'a')

-- generating from data structures generically

-- gmapQ :: forall a. Data a => (forall d. Data d => d -> d) -> a -> a

fields :: [Constr]
fields = gmapQ toConstr (X 5 'c')
