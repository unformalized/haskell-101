{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Advance.Lense (lenTest) where

import Data.IntMap.Lazy (updateWithKey)
import Data.Text

data Address = Address
  { addressCity :: !Text,
    addressStreet :: !Text
  }
  deriving (Show)

data Person = Person
  { personAddress :: !Address,
    personName :: !Text
  }
  deriving (Show)

myAddr :: Address
myAddr = Address "Chengdu" "Tian Hua Street"

liubin :: Person
liubin = Person myAddr "Liubin"

getPersonCity :: Person -> Text
getPersonCity = addressCity . personAddress

setPersonCity :: Text -> Person -> Person
setPersonCity city person =
  person
    { personAddress =
        (personAddress person)
          { addressCity = city
          }
    }

modifyAddressCity :: (Text -> Text) -> Address -> Address
modifyAddressCity f address =
  address
    { addressCity = f (addressCity address)
    }

modifyPersonAddress :: (Address -> Address) -> Person -> Person
modifyPersonAddress f person =
  person
    { personAddress = f (personAddress person)
    }

modifyPersonCity :: (Text -> Text) -> Person -> Person
modifyPersonCity = modifyPersonAddress . modifyAddressCity

setPersonCity2 :: Text -> Person -> Person
setPersonCity2 city = modifyPersonCity (const city)

-- old style lens

data LensOld s a = LensOld
  { lensGetter :: s -> a,
    lensSetter :: (a -> a) -> s -> s
  }

composeLens :: LensOld a b -> LensOld b c -> LensOld a c
composeLens (LensOld get1 set1) (LensOld get2 set2) =
  LensOld
    { lensGetter = get2 . get1,
      lensSetter = set1 . set2
    }

personAddressL :: LensOld Person Address
personAddressL =
  LensOld
    { lensGetter = personAddress,
      lensSetter = \f person -> person {personAddress = f (personAddress person)}
    }

addressCityL :: LensOld Address Text
addressCityL =
  LensOld
    { lensGetter = addressCity,
      lensSetter = \f address -> address {addressCity = f (addressCity address)}
    }

personCityL :: LensOld Person Text
personCityL = personAddressL `composeLens` addressCityL

setPersonCityL :: Text -> Person -> Person
setPersonCityL city = lensSetter personCityL (const city)

-- polymorphic update

type Foo = (Int, Int)

type Goo = (String, Int)

foo2goo (n, m) = (show n, m)

data Bar = Bar
  { car :: String,
    foo :: Foo
  }
  deriving (Show)

setFoo1 :: Int -> Foo -> Foo
setFoo1 n (m1, _) = (m1, n)

setBarFoo :: Foo -> Bar -> Bar
setBarFoo f bar = bar {foo = f}

-- next forms of lenses

newtype Identity a = Identity {runIdentity :: a} deriving (Functor)

newtype Const b a = Const {runConst :: b} deriving (Functor)

type LensModify s a = (a -> Identity a) -> (s -> Identity s)

type LensSetter s a = s -> Const a s

view' :: LensSetter s a -> s -> a
view' lens s = runConst (lens s)

over' :: LensModify s a -> (a -> a) -> s -> s
over' lens f s = runIdentity (lens (Identity . f) s)

personAddressL'' :: LensModify Person Address
personAddressL'' f person =
  (\address -> person {personAddress = address}) <$> f (personAddress person)

-- Identity $
--   person
--     { personAddress = runIdentity $ f $ personAddress person
--     }

viewPersonAddressL'' :: LensSetter Person Address
viewPersonAddressL'' person = Const (personAddress person)


type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set updateF s = set s <$> updateF (get s)

view :: Lens s t a b -> s -> a
view l s = runConst $ l Const s

set :: Lens s t a b -> b -> s -> t
set l a s = runIdentity $ l (const $ Identity a) s

over :: Lens s t a b -> (a -> b) -> s -> t
over lens f s = runIdentity (lens (Identity . f) s)

addressCityL' :: Lens Address Address Text Text
addressCityL' = lens addressCity $ \addr city -> addr {addressCity = city}

personAddressL' :: Lens Person Person Address Address
personAddressL' = lens personAddress $ \person addr -> person {personAddress = addr}

lenTest :: IO ()
lenTest = do
  print $ view (personAddressL' . addressCityL') liubin
  print $ set (personAddressL' . addressCityL') "ShangHai" liubin
