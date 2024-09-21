module Test.Spec.OnlyRawPatch where

import Prelude

import Effect.Class (liftEffect)

import Data.Tuple.Nested ((/\), type (/\))

import Test.Spec (Spec, pending, describe, it, pending', itOnly)
import Test.Spec.Assertions (fail, shouldEqual)

import Noodle.OnlyRawPatch (make, fromToolkit, registerNodeNotFromToolkit, registerNode, registerRawNode, mapNodes, connect, disconnect) as Patch
import Noodle.Node (run) as Node
import Noodle.Node ((<-@), (#->))
import Noodle.Raw.Node (family) as RawNode

import Test.MyToolkit.Toolkit as MyToolkit
import Test.MyToolkit.Node.Sum as Sum
import Test.MyToolkit.Node.Concat as Concat
import Test.MyToolkit.Node.Raw.Concat as RawConcat


spec :: Spec Unit
spec = do

    describe "registering nodes inside the patch" $ do

        it "registering a node not from a toolkit" $ liftEffect $ do
            emptyPatch <- Patch.make "test" unit
            concatNode <- Concat.makeNode
            let
                patchWithNodes =
                    emptyPatch
                        # Patch.registerNodeNotFromToolkit concatNode
                nodesInPatch =
                    Patch.mapNodes RawNode.family patchWithNodes
            (show <$> nodesInPatch) `shouldEqual` [ "concat" ]


        it "registering a node from toolkit by family" $ liftEffect $ do
            emptyPatch <- Patch.fromToolkit MyToolkit.toolkit "test" unit
            concatNode <- Concat.makeNode
            let
                patchWithNodes =
                    emptyPatch
                        # Patch.registerNode concatNode
                nodesInPatch =
                    Patch.mapNodes RawNode.family patchWithNodes
            (show <$> nodesInPatch) `shouldEqual` [ "concat" ]


        it "registering a raw node from toolkit by family" $ liftEffect $ do
            emptyPatch <- Patch.make "test" unit
            concatNode <- RawConcat.makeNode
            let
                patchWithNodes =
                    emptyPatch
                        # Patch.registerRawNode concatNode
                nodesInPatch =
                    Patch.mapNodes RawNode.family patchWithNodes
            (show <$> nodesInPatch) `shouldEqual` [ "concatR" ]

        pending' "nodes are properly grouped by family" $ liftEffect $ do
            pure unit

    describe "connecting & disconnecting nodes inside the patch" $ do

        pending' "it is possible to connect nodes inside the patch" $ liftEffect $ do
            pure unit

        pending' "it is possible to connect and then disconnect nodes inside the patch" $ liftEffect $ do
            pure unit

        it "is possible to connect nodes (case a)" $ liftEffect $ do
            (nodeA :: Sum.Node) <- Sum.makeNode_ { a : 2, b : 3 } { sum : 0 } Sum.sumBoth
            (nodeB :: Sum.Node) <- Sum.makeNode_ { a : 2, b : 3 } { sum : 0 } Sum.sumBoth

            emptyPatch <- Patch.fromToolkit MyToolkit.toolkit "test" unit
            let patchWithNodes = emptyPatch # Patch.registerNode nodeA # Patch.registerNode nodeB

            _ <- Patch.connect Sum.sum_out Sum.b_in nodeA nodeB emptyPatch

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB <- nodeB <-@ Sum.sum_out
            -- `sum` outlet of `nodeA` is connected to the `b` inlet of `nodeB`,
            -- so it's `a (2) + b (3)` from `nodeA` and `a (2) + b (2 + 3)` from `nodeB`
            atSumB `shouldEqual` (2 + 2 + 3)

            pure unit

        it "is possible to connect nodes (case b)" $ liftEffect $ do
            (nodeA :: Sum.Node) <- Sum.makeNode_ { a : 2, b : 3 } { sum : 0 } Sum.sumBoth
            (nodeB :: Sum.Node) <- Sum.makeNode_ { a : 2, b : 3 } { sum : 0 } Sum.sumBoth

            nodeA #-> Sum.a_in /\ 4

            emptyPatch <- Patch.fromToolkit MyToolkit.toolkit "test" unit
            let patchWithNodes =
                    emptyPatch
                    # Patch.registerNode nodeA
                    # Patch.registerNode nodeB

            _ <- Patch.connect Sum.sum_out Sum.b_in nodeA nodeB patchWithNodes

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB <- nodeB <-@ Sum.sum_out
            -- `sum` outlet of `nodeA` is connected to the `b` inlet of `nodeB`,
            -- and just a few lines above we've sent a value of `4` to the `a` inlet of `nodeA`
            -- so it's `a (4) + b (3)` from `nodeA` and `a (2) + b (4 + 3)` from `nodeB`
            atSumB `shouldEqual` (2 + 4 + 3)

            pure unit

        it "is possible to connect nodes and keep sending values" $ liftEffect $ do
            (nodeA :: Sum.Node) <- Sum.makeNode_ { a : 2, b : 3 } { sum : 0 } Sum.sumBoth
            (nodeB :: Sum.Node) <- Sum.makeNode_ { a : 2, b : 3 } { sum : 0 } Sum.sumBoth

            nodeA #-> Sum.a_in /\ 4

            emptyPatch <- Patch.fromToolkit MyToolkit.toolkit "test" unit
            let patchWithNodes = emptyPatch
                    # Patch.registerNode nodeA
                    # Patch.registerNode nodeB

            _ <- Patch.connect Sum.sum_out Sum.b_in nodeA nodeB patchWithNodes

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            -- atSumB <- nodeB <-@ sum_out
            -- atSumB `shouldEqual` (2 + 4 + 3)

            nodeA #-> Sum.a_in /\ 7

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB' <- nodeB <-@ Sum.sum_out
            -- `sum` outlet of `nodeA` is connected to the `b` inlet of `nodeB`,
            -- and just a few lines above we've sent a value of `7` to the `a` inlet of `nodeA`
            -- (..to replace the value of `4` which was sent before)
            -- so it's `a (7) + b (3)` from `nodeA` and `a (2) + b (7 + 3)` from `nodeB`
            atSumB' `shouldEqual` (2 + 7 + 3)

            pure unit


        it "disconnecting works" $ liftEffect $ do
            (nodeA :: Sum.Node) <- Sum.makeNode_ { a : 2, b : 3 } { sum : 0 } Sum.sumBoth
            (nodeB :: Sum.Node) <- Sum.makeNode_ { a : 2, b : 3 } { sum : 0 } Sum.sumBoth

            nodeA #-> Sum.a_in /\ 4

            emptyPatch <- Patch.fromToolkit MyToolkit.toolkit "test" unit
            let patchWithNodes =
                    emptyPatch
                    # Patch.registerNode nodeA
                    # Patch.registerNode nodeB

            nextPatch /\ link <- Patch.connect Sum.sum_out Sum.b_in nodeA nodeB patchWithNodes

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            -- atSumB <- nodeB <-@ sum_out
            -- atSumB `shouldEqual` (2 + 4 + 3)

            _ /\ success <- Patch.disconnect link nextPatch -- nodeA nodeB
            success `shouldEqual` true

            nodeA #-> Sum.a_in /\ 7

            _ <- Node.run nodeA
            _ <- Node.run nodeB

            atSumB' <- nodeB <-@ Sum.sum_out
            -- it should act like the value that was sent after disconnecting was never received
            atSumB' `shouldEqual` (2 + 4 + 3)

            pure unit


    describe "patch state" $ do

        pending' "it is possible to operate with patch state from outside" $ liftEffect $ do
            pure unit

        pending' "it is possible to operate with patch state from inside the node" $ liftEffect $ do
            pure unit

    describe "iterating through nodes" $ do

        pending' "it is possible to iterate through all typed nodes" $ liftEffect $ do
            pure unit

        pending' "it is possible to iterate through all raw nodes" $ liftEffect $ do
            pure unit

    describe "iterating through links" $ do

        pending' "it is possible to iterate through all links in the patch" $ liftEffect $ do
            pure unit