{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
-- |

module Generic.DataPrettyPrint where

import Data.Tree
import GHC.Generics
import Text.PrettyPrint.GenericPretty

-- deriving instance Generic (Tree a)

instance Out a => Out (Tree a)

directory :: Tree String
directory = Node "Home"
  [ Node "picture" [Node "travel" [], Node "family" []]
  , Node "video" [Node "Fast and Furious" [], Node "Ture lies" []]
  , Node "music" [Node "My love" [], Node "Destiny" []]
  ]

testPrettyPrintDirectory = pp directory
