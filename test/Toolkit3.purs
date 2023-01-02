module Test.Toolkit3 where

import Prelude

import Effect.Class (liftEffect)
import Effect.Console (log) as Console

import Data.List ((:))
import Data.List as List
import Data.Tuple.Nested ((/\), type (/\))

import Type.Data.Symbol (reflectSymbol, class IsSymbol)

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

        let toolkit =
                Toolkit.from "test"
                    { foo :
                        unit
                        /\ { foo : "aaa", bar : "bbb", c : 32 }
                        /\ { out : false }
                        /\ Fn.make "foo" (pure unit)
                    }

        it "spawning works" $ do

            node <- Toolkit.spawn toolkit (Node.Family :: Node.Family "foo")

            state <- Node.state node
            state `shouldEqual` unit

            atC <- Node.inputs node <#> _.c
            atC `shouldEqual` 32

            pure unit


        it "getting family list" $ do
            Toolkit.nodeFamilies toolkit `shouldEqual` ( "foo" : List.Nil )
            -- let
            --     myFn :: (forall f. IsSymbol f => Node.Family f) -> String
            --     myFn f = reflectSymbol f
            -- (?wh <$> Toolkit.nodeFamilies' toolkit) `shouldEqual` ( "foo" : List.Nil )
