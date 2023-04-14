module Test.Node2 where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log) as Console


import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Signal (expectFn, expect)

import Data.List ((:))
import Data.List (List(..)) as List
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (bimap)
import Data.SOrder (SOrder, type (:::), T)
import Type.Proxy (Proxy(..))

-- import Noodle.Node.Shape (noInlets, noOutlets) as Shape
-- import Noodle.Node ((<~>), (+>), (<+))
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Fn2 (Fn)
import Noodle.Fn2 as Fn
import Noodle.Fn2.Process as P
import Noodle.Id (Family(..)) as Node
import Noodle.Id (Input(..), Output(..), InputR(..) ) as Fn
import Noodle.Id (reflect')

import Signal ((~>), Signal)
import Signal as Signal
import Signal.Channel as Ch
import Signal.Time as SignalT


_sum = Node.Family :: Node.Family "sum"


_aI = Fn.Input 1 :: Fn.Input "a"
_bI = Fn.Input 2 :: Fn.Input "b"
_sumO = (Fn.Output 1 :: Fn.Output "sum")


io = Proxy :: _ ("a" ::: "b" ::: T)
oo = Proxy :: _ ("sum" ::: T)


spec :: Spec Unit
spec = do

    describe "creating & initial values" $ do

        it "is initialized properly" $ do
            node <- Node.make _sum unit io oo { a : 2, b : 3 } { sum : 0 } $ pure unit

            state <- Node.state node
            state `shouldEqual` unit

            atA <- Node.inputs node <#> _.a
            atA `shouldEqual` 2
            atA' <- node `Node.atI` _aI
            atA' `shouldEqual` 2
            atA'' <- node `Node._at` _.a
            atA'' `shouldEqual` 2

            atB <- Node.inputs node <#> _.b
            atB `shouldEqual` 3
            atB' <- node `Node.atI` _bI
            atB' `shouldEqual` 3
            atB'' <- node `Node._at` _.b
            atB'' `shouldEqual` 3

            atSum <- Node.outputs node <#> _.sum
            atSum `shouldEqual` 0
            atSum' <- node `Node.atO` _sumO
            atSum' `shouldEqual` 0
            atSum'' <- node `Node.at_` _.sum
            atSum'' `shouldEqual` 0

            pure unit

        it "function is performed properly" $ do

            node <-
                Node.make _sum unit io oo { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send (Fn.Output 0 :: Fn.Output "sum") $ a + b

            atSum <- node `Node.at_` _.sum
            atSum `shouldEqual` 0

            _ <- Node.run node

            atSumAfter <- node `Node.at_` _.sum
            atSumAfter `shouldEqual` 5

            pure unit

    describe "shapes" $ do

        it "is possible to extract shape" $ do
            node <-
                Node.make _sum unit io oo { a : 2, b : 3 } { sum : 0 } $ pure unit

            (reflect' <$> Node.inputsShape node) `shouldEqual` ( "a" : "b" : List.Nil )
            (reflect' <$> Node.outputsShape node) `shouldEqual` ( "sum" : List.Nil )

            (bimap (map reflect') (map reflect') $ Node.shape node)
                `shouldEqual`
                (( "a" : "b" : List.Nil ) /\ ( "sum" : List.Nil ))


    describe "connecting & disconnecting" $ do

        it "is possible to connect nodes" $ do
            nodeA <-
                Node.make _sum unit io oo { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send (Fn.Output 0 :: Fn.Output "sum") $ a + b

            nodeB <-
                Node.make _sum unit io oo { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send (Fn.Output 0 :: Fn.Output "sum") $ a + b

            -- Node.with nodeA $ P.sendIn _aI 4
            Node.sendIn nodeA _aI 4

            _ <- Node.connect
                    (Fn.Output 0 :: Fn.Output "sum")
                    _bI
                    identity
                    nodeA
                    nodeB

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB <- nodeB `Node.at_` _.sum
            atSumB `shouldEqual` (4 + 3 + 2)

            pure unit


        it "is possible to connect nodes and keep sending values" $ do
            nodeA <-
                Node.make _sum unit io oo { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send _sumO $ a + b

            nodeB <-
                Node.make _sum unit io oo { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send _sumO $ a + b

            -- Node.with nodeA $ P.sendIn _aI 4
            Node.sendIn nodeA _aI 4

            _ <- Node.connect
                    _sumO
                    _bI
                    identity
                    nodeA
                    nodeB

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB <- nodeB `Node.at_` _.sum
            atSumB `shouldEqual` (4 + 3 + 2)

            -- Node.with nodeA $ P.sendIn _aI 7
            Node.sendIn nodeA _aI 7

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB' <- nodeB `Node.at_` _.sum
            atSumB' `shouldEqual` (7 + 3 + 2)

            pure unit

        it "disconnecting works" $ do
            nodeA <-
                Node.make _sum unit io oo { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send _sumO $ a + b

            nodeB <-
                Node.make _sum unit io oo { a : 2, b : 3 } { sum : 0 }
                    $ do
                        a <- P.receive _aI
                        b <- P.receive _bI
                        P.send _sumO $ a + b

            -- Node.with nodeA $ P.sendIn _aI 4
            Node.sendIn nodeA _aI 4

            link <- Node.connect
                    _sumO
                    _bI
                    identity
                    nodeA
                    nodeB

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB <- nodeB `Node.at_` _.sum
            atSumB `shouldEqual` (4 + 3 + 2)

            success <- Node.disconnect link nodeA nodeB
            success `shouldEqual` true

            -- Node.with nodeA $ P.sendIn _aI 7
            Node.sendIn nodeA _aI 7

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB' <- nodeB `Node.at_` _.sum
            atSumB' `shouldEqual` (4 + 3 + 2)

            pure unit

        -- TODO: test hashes
