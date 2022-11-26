-- |
module Torrent.Bencode where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import Prelude as Std

-- benInt :: Parser Int
-- benInt = P.integer _ _ _
