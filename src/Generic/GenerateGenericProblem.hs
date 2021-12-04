{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Generic.GenerateGenericProblem where

import Data.Word
import GHC.Generics
import Data.ByteString
import qualified Data.Map as M
import qualified Data.Set as S

data D_word
data C_word

instance Datatype D_word where
  datatypeName _ = "Word"
  moduleName _ = "GHC.Word"

instance Constructor C_word where
  conName _ = ""

{-
instance Generic Word where
  type Rep Word = D1 D_word (C1 C_word (S1 NoSelector (Rec0 Word)))
  from x = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = x
-}

instance Generic ByteString where
  type Rep ByteString = Rep [Word8]
  from bs = from (unpack bs)
  to w = pack $ (to w)

instance Ord a => Generic (S.Set a) where
  type Rep (S.Set a) = Rep [a]
  from a = from (S.toList a)
  to a = S.fromList (to a)

instance Ord k => Generic (M.Map k v) where
  type Rep (M.Map k v) = Rep [(k, v)]
  from a = from (M.toList a)
  to a = M.fromList (to a)

