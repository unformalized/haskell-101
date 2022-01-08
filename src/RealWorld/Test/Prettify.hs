module RealWorld.Test.Prettify where

import Control.Monad (liftM, liftM2)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, choose, elements, oneof)
import Prelude hiding ((<>))

data Doc
  = Empty
  | Char Char
  | Text String
  | Line
  | Concat Doc Doc
  | Union Doc Doc
  deriving (Show, Eq)

data Ternary
  = Yes
  | No
  | Unknown
  deriving (Eq, Show)

-- 实现 Aribitrary 类型可以让 quickCheck 自动生成随机数据
-- Aribitrary 提供三个函数让我们如何构造随机数据
{-
  elements :: [a] -> Gen a
  choose   :: Random a => (a, a) -> Gen a
  oneof    :: [Gen a] -> Gen a
-}

{- instance Arbitrary Ternary where
  arbitrary = elements [Yes, No, Unknown] -}

instance Arbitrary Ternary where
  arbitrary = do
    n <- choose (0, 2) :: Gen Int
    return $ case n of
      0 -> Yes
      1 -> No
      _ -> Unknown

{-
instance Arbitrary Doc where
  arbitrary = do
    n <- choose (1, 6) :: Gen Int
    case n of
      1 -> return Empty
      2 -> do
        x <- arbitrary
        return (Char x)
      3 -> do
        x <- arbitrary
        return (Text x)
      4 -> return Line
      5 -> do
        x <- arbitrary
        y <- arbitrary
        return (Concat x y)
      6 -> do
        x <- arbitrary
        y <- arbitrary
        return (Union x y)
 -}
instance Arbitrary Doc where
  arbitrary =
    oneof
      [ return Empty,
        liftM Char arbitrary,
        liftM Text arbitrary,
        return Line,
        liftM2 Concat arbitrary arbitrary,
        liftM2 Union arbitrary arbitrary
      ]

empty :: Doc
empty = Empty

(<>) :: Doc -> Doc -> Doc
(<>) Empty doc = doc
(<>) doc Empty = doc
(<>) doc1 doc2 = Concat doc1 doc2

prop_empty_id :: Doc -> Bool
prop_empty_id x =
  empty <> x == x
    && x <> empty == x

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty
