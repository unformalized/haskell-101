module RealWorld.ParsePgm.Parse where

import qualified Data.ByteString.Lazy as L
import GHC.Int (Int64)

data ParserState = ParserState
  { string :: L.ByteString,
    offset :: Int64
  }
  deriving (Show)
