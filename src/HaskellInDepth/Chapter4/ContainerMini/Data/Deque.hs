{-# LANGUAGE NoImplicitPrelude #-}

module HaskellInDepth.Chapter4.ContainerMini.Data.Deque where

import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..))
import Data.Sequence hiding (empty)
import qualified Data.Sequence as Seq
import Prelude (error, otherwise, (-))

newtype Deque a = Deque (Seq a)

empty :: Deque a
empty = Deque Seq.empty

isEmpty :: Deque a -> Bool
isEmpty (Deque seq) = Seq.null seq

front :: Deque a -> Maybe a
front (Deque seq)
  | Seq.null seq = Nothing
  | otherwise = lookup 0 seq

back :: Deque a -> Maybe a
back (Deque seq)
  | Seq.null seq = Nothing
  | otherwise = seq !? (length seq - 1)

push_front :: a -> Deque a -> Deque a
push_front a (Deque seq) = Deque (a <| seq)

push_back :: a -> Deque a -> Deque a
push_back a (Deque seq) = Deque (seq |> a)

pop_front :: Deque a -> Deque a
pop_front (Deque seq) =
  case viewl seq of
    EmptyL -> error "Empty Deque"
    (a :< seq') -> Deque seq'

pop_back :: Deque a -> Deque a
pop_back (Deque seq) =
  case viewr seq of
    EmptyR -> error "Empty Deque"
    (seq' :> a) -> Deque seq'
