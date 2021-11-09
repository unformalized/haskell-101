-- | 代理类型
{-# LANGUAGE PolyKinds #-}

module Advance.Proxy where

import Data.Proxy

newtype Tagged (s :: k) b = Tagged { unTagged :: b }

taggedA :: Tagged Maybe Int
taggedA = Tagged 4

idProxy :: Proxy t -> t -> t
idProxy = flip asProxyTypeOf
