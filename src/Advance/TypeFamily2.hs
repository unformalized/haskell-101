{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Advance.TypeFamily2 where

type family Elem a :: *
type instance Elem [e] = e

class (Elem ce ~ e) => Collection e ce where
  empty :: ce
  insert :: e -> ce -> ce
  member :: e -> ce -> Bool
