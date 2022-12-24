module Test.Node2 where

import Prelude


import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Signal (expectFn, expect)

import Data.Tuple.Nested ((/\), type (/\))

-- import Noodle.Node.Shape (noInlets, noOutlets) as Shape
-- import Noodle.Node ((<~>), (+>), (<+))
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Fn2 (Fn)
import Noodle.Fn2 as Fn

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


spec :: Spec Unit
spec = do

    describe "creating & initial values" $ do

        it "is initialized properly" $ do
            let
                fn =
                    Fn.make "sum" $ do
                        pure unit
            _ <- Node.make ("sum" /\ 1) unit { a : 2, b : 3 } { sum : 0 } fn
            pure unit