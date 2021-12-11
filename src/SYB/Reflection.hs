module SYB.Reflection where

import Data.Data (Constr)

data Datatype = Datatype {tycon :: String, dataRep :: DataRep}

data DataRep
  = AlgRep [Constr]
  | IntRep
  | FloatRep
  | CharRep
  | NoRep
