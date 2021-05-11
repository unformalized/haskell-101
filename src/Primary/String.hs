module Primary.String where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Char
import Data.String

abc :: String
abc = "abc"

abs' = T.map toUpper (fromString abc)











