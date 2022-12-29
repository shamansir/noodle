module Test.Patch2 where

import Prelude

import Effect.Class (liftEffect)
import Effect.Console (log) as Console

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
import Noodle.Patch2 (Patch)
import Noodle.Patch2 as Patch

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


_foo = (Node.Family :: Node.Family "foo")
_bar = (Node.Family :: Node.Family "bar")


spec :: Spec Unit
spec = do

    describe "patch" $ do

        it "adding node to the patch works" $ do

            let toolkit =
                    Toolkit.from
                        { foo :
                            unit
                            /\ { foo : "aaa", bar : "bbb", c : 32 }
                            /\ { out : false }
                            /\ Fn.make "foo" (pure unit)
                        , bar :
                            unit
                            /\ { a : "aaa", b : "bbb", c : 32 }
                            /\ { x : false }
                            /\ Fn.make "bar" (pure unit)
                        }

            node <- Toolkit.spawn toolkit _foo

            let
                patch = Patch.init toolkit # Patch.registerNode node

            Patch.howMany _foo patch `shouldEqual` 1
            Patch.howMany _bar patch `shouldEqual` 0

            pure unit
