module Test.Toolkit3 where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))

import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Signal (expectFn, expect)

-- import Noodle.Node.Shape (noInlets, noOutlets) as Shape
-- import Noodle.Node ((<~>), (+>), (<+))
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Fn2 (Fn)
import Noodle.Fn2 as Fn

import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3 as Toolkit

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


spec :: Spec Unit
spec = do

    describe "toolkit" $ do

        it "spawning works" $ do

            let toolkit =
                    Toolkit.from
                        { foo :
                            unit
                            /\ { foo : "aaa", bar : "bbb", c : 32 }
                            /\ { out : false }
                            /\ Fn.make "foo" (pure unit)
                        }

            node <- Toolkit.spawn toolkit (Toolkit.Family "foo" :: Toolkit.Family "foo")

            state <- Node.state node
            state `shouldEqual` unit

            atC <- Node.inputs node <#> _.c
            atC `shouldEqual` 32

            pure unit
