{-# LANGUAGE StandaloneDeriving #-}

module Primary.StandaloneD where

import Primary.TypeclassImpl

deriving instance (Show a) => Show (Foo a)
