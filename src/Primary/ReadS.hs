module Primary.ReadS where

newtype Identity a = Identity { runIdentity :: a }

instance (Read a) => Read (Identity a) where
  readsPrec d = readParen (d > 10) $ \r ->
    [(Identity x, t) | ("Identity", s) <- lex r, (x, t) <- readsPrec 11 s]



