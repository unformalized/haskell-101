module Advance.DynamicType where
  
import Data.Dynamic
import Control.Applicative
import Unsafe.Coerce
import Data.Typeable
import GHC.Prim
import GHC.Exts (Any)

n5 :: Dynamic
n5 = toDyn (5 :: Int)

idNum :: Dynamic
idNum = toDyn (id :: Int -> Int)

matchZero :: Dynamic -> Maybe Int
matchZero d =
 case fromDynamic d :: Maybe Int of
   Nothing -> Nothing
   Just n  -> if n == 0
     then return 0
     else Nothing

matchBool :: Dynamic -> Maybe Int
matchBool b =
 case fromDynamic b :: Maybe Bool of
   Nothing -> Nothing
   Just b'  -> if b' then return 1 else return 0
   

dynamicMatch :: Dynamic -> Maybe Int
dynamicMatch a = foldl (<|>) Nothing [matchZero a, matchBool a]

-- fix :: (a -> b) -> b
-- fix f = (\x -> f (unsafeCoerce x x)) (\x -> f (unsafeCoerce x x))


-- self Dynamic
type SelfObj = Any
data DynamicSelf = DynamicSelf TypeRep SelfObj

toDynSelf :: Typeable a => a -> DynamicSelf
toDynSelf v = DynamicSelf (typeOf v) (unsafeCoerce v)


fromDynamicSelf :: Typeable a => DynamicSelf -> Maybe a
fromDynamicSelf (DynamicSelf t v) = case unsafeCoerce v of
                                      r | t == typeOf r -> Just r
                                        | otherwise -> Nothing


