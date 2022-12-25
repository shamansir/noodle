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
import Noodle.Fn2.Flow as Fn
import Noodle.Fn2.Process as P

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


spec :: Spec Unit
spec = do

    describe "creating & initial values" $ do

        it "is initialized properly" $ do
            node <- Node.make ("sum" /\ 1) unit { a : 2, b : 3 } { sum : 0 } $ pure unit

            state <- Node.state node
            state `shouldEqual` unit

            atA <- Node.inputs node <#> _.a
            atA `shouldEqual` 2
            atA' <- node `Node.atI` (Fn.Input :: Fn.Input "a")
            atA' `shouldEqual` 2
            atA'' <- node `Node._at` _.a
            atA'' `shouldEqual` 2

            atB <- Node.inputs node <#> _.b
            atB `shouldEqual` 3
            atB' <- node `Node.atI` (Fn.Input :: Fn.Input "b")
            atB' `shouldEqual` 3
            atB'' <- node `Node._at` _.b
            atB'' `shouldEqual` 3

            atSum <- Node.outputs node <#> _.sum
            atSum `shouldEqual` 0
            atSum' <- node `Node.atO` (Fn.Output :: Fn.Output "sum")
            atSum' `shouldEqual` 0
            atSum'' <- node `Node.at_` _.sum
            atSum'' `shouldEqual` 0

            pure unit

        it "function is performed properly" $ do

            -- let (test :: _) = _ { foo = _ }

            node <-
                Node.make ("sum" /\ 1) unit { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive (Fn.Input :: Fn.Input "a")
                        b <- P.receive (Fn.Input :: Fn.Input "b")
                        P.send (Fn.Output :: Fn.Output "sum") $ a + b

            atSum <- node `Node.at_` _.sum
            atSum `shouldEqual` 0

            _ <- Node.run node

            atSumAfter <- node `Node.at_` _.sum
            atSumAfter `shouldEqual` 5

            pure unit
