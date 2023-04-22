module Test.SOrder where

import Prelude

import Effect.Console as Console
import Effect.Class (liftEffect)

import Test.Spec (Spec, pending, describe, it, pending')
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import Data.SOrder (type (:::))
import Data.SOrder as SO
import Data.KeyHolder as KH
import Data.Symbol (SProxy, class IsSymbol, reflectSymbol)


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

    describe "using KeyHolder" $ do

        it "properly sorts keys of record" $ do
            let order = SO.instantiateImpl (Proxy :: _ ("foo" ::: "bar" ::: "buz" ::: SO.T))
            liftEffect $ Console.log $ show order
            ((\convs -> withConvS convs reflectSymbol) <$> (KH.orderedKeys (Proxy :: Proxy SProxy) order { buz : "test", foo : 2, bar : false } :: Array ConvS)) `shouldEqual` [ "foo", "bar", "buz" ]

        it "properly sorts keys of the row" $ do
            let order = SO.instantiateImpl (Proxy :: _ ("foo" ::: "bar" ::: "buz" ::: SO.T))
            ((\conv -> withConv conv reflectSymbol) <$> (KH.orderedKeysFromRow (Proxy :: Proxy SProxy) order (Proxy :: _ ( buz :: String, foo :: Int, bar :: Boolean )) :: Array Conv)) `shouldEqual` [ "foo", "bar", "buz" ]

        it "properly sorts keys of record with index" $ do
            let order = SO.instantiateImpl (Proxy :: _ ("foo" ::: "bar" ::: "buz" ::: SO.T))
            liftEffect $ Console.log $ show order
            ((\conv -> withConv conv reflectSymbol) <$> (KH.orderedKeys (Proxy :: Proxy SymH) order { buz : "test", foo : 2, bar : false } :: Array Conv)) `shouldEqual` [ "foo", "bar", "buz" ]


        it "properly sorts keys of record with index. p.2" $ do
            let order = SO.instantiateImpl (Proxy :: _ ("bar" ::: "foo" ::: "buz" ::: SO.T))
            liftEffect $ Console.log $ show order
            ((\conv -> withConv conv reflectSymH) <$> (KH.orderedKeys (Proxy :: Proxy SymH) order { buz : "test", foo : 2, bar : false } :: Array Conv)) `shouldEqual` [ "bar0", "foo1", "buz2" ]


data SymH (s :: Symbol) = SymH Int


reflectSymH :: forall sym. IsSymbol sym => SymH sym -> String
reflectSymH (SymH n) = reflectSymbol (Proxy :: _ sym) <> show n


newtype ConvS = ConvS (forall r. (forall sym. IsSymbol sym => SProxy sym -> r) -> r)
newtype Conv = Conv (forall r. (forall sym. IsSymbol sym => SymH sym -> r) -> r)


holdConvS :: forall sym. IsSymbol sym => SProxy sym -> ConvS
holdConvS sym = ConvS (_ $ sym)


withConvS :: forall r. ConvS -> (forall sym. IsSymbol sym => SProxy sym -> r) -> r
withConvS (ConvS fn) = fn


holdConv :: forall sym. IsSymbol sym => SymH sym -> Conv
holdConv sym = Conv (_ $ sym)


withConv :: forall r. Conv -> (forall sym. IsSymbol sym => SymH sym -> r) -> r
withConv (Conv fn) = fn


instance KH.Holder SProxy ConvS where
    hold = holdConvS
    extract = withConvS


instance KH.Holder SymH Conv where
    hold = holdConv
    extract = withConv


instance KH.ReifyOrderedTo SymH where
    reifyAt :: forall a sym. IsSymbol sym => Int -> Proxy sym -> a -> SymH sym
    reifyAt n _ _ = SymH n


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
