module Test.SOrder where

import Prelude


import Test.Spec (Spec, pending, describe, it, pending')
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import Data.SOrder (type (:::))
import Data.SOrder as SO


type ThreeItems :: SO.SOrder
type ThreeItems = "foo" ::: "bar" ::: "lll" ::: SO.T


type ACB :: SO.SOrder
type ACB = "a" ::: "c" ::: "b" ::: SO.T


type Empty :: SO.SOrder
type Empty = SO.T


type One :: SO.SOrder
type One = "first" ::: SO.T


spec :: Spec Unit
spec = do

    describe "ordering with symbols" $ do

        it "properly calculates length" $ do
            SO.length (Proxy :: _ ThreeItems) `shouldEqual` 3
            SO.length (Proxy :: _ ACB) `shouldEqual` 3
            SO.length (Proxy :: _ Empty) `shouldEqual` 0
            SO.length (Proxy :: _ One) `shouldEqual` 1

        it "properly calculates index" $ do
            SO.index (Proxy :: _ SO.T) `shouldEqual` -1
            SO.index (Proxy :: _ ("foo" ::: SO.T)) `shouldEqual` 0
            SO.index (Proxy :: _ ("foo" ::: "bar" ::: SO.T)) `shouldEqual` 1
            SO.index (Proxy :: _ ("foo" ::: "bar" ::: "buz" ::: SO.T)) `shouldEqual` 2

        it "properly collects values" $ do
            SO.reflect (Proxy :: _ SO.T) `shouldEqual` []
            SO.reflect (Proxy :: _ ("foo" ::: SO.T)) `shouldEqual` [ "foo" ]
            SO.reflect (Proxy :: _ ("foo" ::: "bar" ::: SO.T)) `shouldEqual` [ "foo", "bar" ]
            SO.reflect (Proxy :: _ ("foo" ::: "bar" ::: "buz" ::: SO.T)) `shouldEqual` [ "foo", "bar", "buz" ]
            SO.reflect (Proxy :: _ ACB) `shouldEqual` [ "a", "c", "b" ]

        it "sorts properly" $ do
            SO.sort (Proxy :: _ ThreeItems) [ "bar", "foo", "lll" ] `shouldEqual` [ "foo", "bar", "lll" ]
            SO.sort (Proxy :: _ ACB) [ "a", "b", "c" ] `shouldEqual` [ "a", "c", "b" ]
            SO.sort (Proxy :: _ ACB) [ "a", "b", "c", "d" ] `shouldEqual` [ "a", "c", "b", "d" ]
            SO.sort (Proxy :: _ One) [ "second", "third", "fourth", "first", "seventh" ] `shouldEqual` [ "first", "second", "third", "fourth", "seventh" ]

        {-}
        describe "order values" $ do

            it "three items case" $ do
                SO.order (Proxy :: _ ThreeItems) "a" `shouldEqual` -1
                SO.order (Proxy :: _ ThreeItems) "foo" `shouldEqual` 1
                -- SO.order (Proxy :: _ ThreeItems) "bar" `shouldEqual` 2
                -- SO.order (Proxy :: _ ThreeItems) "lll" `shouldEqual` 1

            it "several rows" $ do
                pure unit
            -}
