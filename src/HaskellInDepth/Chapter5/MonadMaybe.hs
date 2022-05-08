module HaskellInDepth.Chapter5.MonadMaybe where

import Text.Read (readMaybe)

doubleStrNumber1 :: String -> Maybe Int
doubleStrNumber1 str =
  case readMaybe str of
    Just x -> Just (2 * x)
    Nothing -> Nothing

doubleStrNumber2 :: String -> Maybe Int
doubleStrNumber2 str = (2 *) `fmap` readMaybe str

plusStrNumbers :: (Num a, Read a) => String -> String -> Maybe a
plusStrNumbers s1 s2 = (+) <$> readMaybe s1 <*> readMaybe s2

type Name = String

type Phone = String

type Location = String

type PhoneNumbers = [(Name, Phone)]

type Locations = [(Phone, Location)]

locateByName :: PhoneNumbers -> Locations -> Name -> Maybe Location
locateByName pnumbers locs name =
  lookup name pnumbers >>= flip lookup locs
