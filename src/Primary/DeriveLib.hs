module Primary.DeriveLib where

import Data.Derive.Instance.Arities ()
import Data.Derive.Class.Arities ( Arities(arities) )
import Data.Derive.Arities ()
import Data.DeriveTH ()


test1 = arities True
-- test2 = arities (5,True)
-- test3 = arities [1]





