-- |
module Advance.LensStart where

data Foo = Foo
  { bar :: (Int, Int),
    baz :: Char
  }
  deriving (Show)

type Lens s a = (a -> a) -> s -> s

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens get update f s = update s (f $ get s)

barL :: Lens Foo (Int, Int)
barL = lens getBar setBar

getBar :: Foo -> (Int, Int)
getBar = bar

setBar :: Foo -> (Int, Int) -> Foo
setBar foo b = foo {bar = b}

_2 :: Lens (a, b) b
_2 = lens get_2 set_2

get_2 :: (a, b) -> b
get_2 = snd

set_2 :: (a, b) -> b -> (a, b)
set_2 (a, _) b = (a, b)

view :: Lens s a -> s -> a
view l s = undefined $ l id s

set :: Lens s a -> a -> s -> s
set l a = l (const a)

myTuple :: (Int, Int)
myTuple = (1, 1)

myFoo :: Foo
myFoo = Foo myTuple 'a'

testLens :: IO ()
testLens = do
  -- print $ view _2 myTuple
  print $ set (barL . _2) 10 myFoo
