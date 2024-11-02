module Test.Napoxiao where

import Prelude


import Effect (Effect)
import ExpectInferred (expectInferred)
import Type.Prelude (Proxy(..), RProxy, SProxy)
import Record.Xiaomian as X
import Record.Naporitan as N

proxies = N.reflectRecordProxy ::
  { apple :: Proxy Int
  , banana :: Proxy String
  }

data MyThing a b c d e f g = MyThing

instance myThingReflectProxy :: N.ReflectProxy (MyThing a b c d e f g) where
  reflectProxy = MyThing

things = N.reflectRecordProxy ::
  { apple :: MyThing Int Int Int Int Int Int Int
  , banana :: MyThing Unit Unit Unit Unit Unit Unit Unit
  }


test :: Unit
test = do
  let
    expected = Proxy :: Proxy (RProxy ( a :: SProxy "a", b :: SProxy "b", c :: SProxy "c" ))
    actual = X.getKeysRow { a: 1, b: 2, c: 3 }
  expectInferred expected actual

test2 :: Unit
test2 = do
  let
    expected = Proxy :: Proxy ({ a :: SProxy "a", b :: SProxy "b", c :: SProxy "c" })
    actual = X.getKeysRecord { a: 1, b: 2, c: 3 }
  expectInferred expected actual

test3 :: Unit
test3 = do
  let
    expected = Proxy :: Proxy ({ a :: SProxy "a", b :: SProxy "b", c :: SProxy "c" })
    actual = X.getKeysRecord' (Proxy :: Proxy { a :: Int, b :: Int, c :: Int })
  expectInferred expected actual

keys ::
  { a :: SProxy "a"
  , b :: SProxy "b"
  , c :: SProxy "c"
  }
keys = X.getKeysRecord' (Proxy :: Proxy { a :: Int, b :: Int, c :: Int })

main :: Effect Unit
main = pure unit