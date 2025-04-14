module Test.Spec.Patch where

import Prelude

import Effect.Class (liftEffect)

import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)

import Control.Monad.Extra (whenJust)

import Signal (Signal)
import Signal (get) as Signal
import Signal.Channel  as Channel

import Data.Tuple.Nested ((/\), type (/\))

import Test.Spec (Spec, pending, describe, it, pending', itOnly)
import Test.Spec.Assertions (fail, shouldEqual)

import Noodle.Id (familyR) as Id
import Noodle.Patch (Patch)
import Noodle.Patch as Patch
import Noodle.Node (run, state, setState) as Node
import Noodle.Node ((<-@), (#->))
import Noodle.Raw.Node (family) as RawNode
import Noodle.Toolkit (loadFromPatch) as Toolkit
import Noodle.Repr.StRepr (class StRepr, from, to) as StRepr

import Example.Toolkit.Minimal.Toolkit as MinimalToolkit
import Example.Toolkit.Minimal.ChRepr (MinimalVRepr)
import Example.Toolkit.Minimal.ChRepr (MinimalVRepr(..)) as Ch
import Example.Toolkit.Minimal.StRepr (MinimalStRepr)
import Example.Toolkit.Minimal.StRepr (MinimalStRepr(..)) as St
import Example.Toolkit.Minimal.Node.Sum as Sum
import Example.Toolkit.Minimal.Node.Concat as Concat
import Example.Toolkit.Minimal.Node.ModifiesPatch as ModifiesPatch
import Example.Toolkit.Minimal.Node.Raw.Concat as RawConcat
import Example.Toolkit.Minimal.PatchState (State(..), default) as Patch


spec :: Spec Unit
spec = do

    describe "registering nodes inside the patch" $ do

        it "registering a node not from a toolkit" $ liftEffect $ do
            emptyPatch <- Patch.make "test" unit
            concatNode <- Concat.makeNode
            let
                (patchWithNodes :: Patch _ _ MinimalStRepr MinimalVRepr _) =
                    emptyPatch
                        # Patch.registerGivenNode concatNode
                nodesInPatch =
                    Patch.mapAllNodes RawNode.family patchWithNodes
            (show <$> nodesInPatch) `shouldEqual` [ "concat" ]


        it "registering a node from toolkit by family" $ liftEffect $ do
            emptyPatch <- Patch.fromToolkit MinimalToolkit.toolkit "test" unit
            concatNode <- Concat.makeNode
            let
                patchWithNodes =
                    emptyPatch
                        # Patch.registerNode concatNode
                nodesInPatch =
                    Patch.mapAllNodes RawNode.family patchWithNodes
            (show <$> nodesInPatch) `shouldEqual` [ "concat" ]


        it "registering a raw node from toolkit by family" $ liftEffect $ do
            emptyPatch <- Patch.make "test" unit
            concatNode <- RawConcat.makeNode
            let
                patchWithNodes =
                    emptyPatch
                        # Patch.registerRawNode concatNode
                nodesInPatch =
                    Patch.mapAllNodes RawNode.family patchWithNodes
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

            emptyPatch <- Patch.fromToolkit MinimalToolkit.toolkit "test" unit
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

            emptyPatch <- Patch.fromToolkit MinimalToolkit.toolkit "test" unit
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

            emptyPatch <- Patch.fromToolkit MinimalToolkit.toolkit "test" unit
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

            emptyPatch <- Patch.fromToolkit MinimalToolkit.toolkit "test" unit
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

        it "it is possible to operate with patch state from inside a node" $ liftEffect $ do
            (modifiesPatch :: ModifiesPatch.Node) <- ModifiesPatch.makeNode

            emptyPatch <- Patch.fromToolkit MinimalToolkit.toolkit "test" Patch.default
            let patchWithNodes =
                    emptyPatch
                    # Patch.registerNode modifiesPatch

            (curState :: Patch.State) <- Patch.getState patchWithNodes

            (modPatchState :: ModifiesPatch.State) <- Node.state modifiesPatch

            let (mbModPatchState :: Maybe ModifiesPatch.State) = Toolkit.loadFromPatch MinimalToolkit.minimalTk (Id.familyR ModifiesPatch._modifiesPatch) curState modPatchState
            whenJust mbModPatchState $ flip Node.setState modifiesPatch
            _ <- Patch.trackStateChangesFrom MinimalToolkit.minimalTk modifiesPatch patchWithNodes

            modifiesPatch #-> ModifiesPatch.a_in /\ 4
            modifiesPatch #-> ModifiesPatch.b_in /\ 3

            _ <- Node.run modifiesPatch

            (patchState :: Patch.State) <- Patch.getState patchWithNodes

            patchState `shouldEqual` (Patch.State $ { intVal : 7, strVal : "7*" })

        it "it is possible to operate with patch state from inside two nodes" $ liftEffect $ do
            (modifiesPatchA :: ModifiesPatch.Node) <- ModifiesPatch.makeNode
            (modifiesPatchB :: ModifiesPatch.Node) <- ModifiesPatch.makeNode

            emptyPatch <- Patch.fromToolkit MinimalToolkit.toolkit "test" Patch.default
            let patchWithNodes =
                    emptyPatch
                    # Patch.registerNode modifiesPatchA
                    # Patch.registerNode modifiesPatchB

            (curState :: Patch.State) <- Patch.getState patchWithNodes

            (modPatchAState :: ModifiesPatch.State) <- Node.state modifiesPatchA

            let (mbModPatchAState :: Maybe ModifiesPatch.State) = Toolkit.loadFromPatch MinimalToolkit.minimalTk (Id.familyR ModifiesPatch._modifiesPatch) curState modPatchAState
            whenJust mbModPatchAState $ flip Node.setState modifiesPatchA
            _ <- Patch.trackStateChangesFrom MinimalToolkit.minimalTk modifiesPatchA patchWithNodes
            _ <- Patch.trackStateChangesFrom MinimalToolkit.minimalTk modifiesPatchB patchWithNodes

            modifiesPatchA #-> ModifiesPatch.a_in /\ 4
            modifiesPatchA #-> ModifiesPatch.b_in /\ 3

            modifiesPatchB #-> ModifiesPatch.a_in /\ 13
            modifiesPatchB #-> ModifiesPatch.b_in /\ 20

            _ <- Node.run modifiesPatchA
            _ <- Node.run modifiesPatchB

            (patchState :: Patch.State) <- Patch.getState patchWithNodes

            patchState `shouldEqual` (Patch.State $ { intVal : 33, strVal : "33*" })

    describe "iterating through nodes" $ do

        pending' "it is possible to iterate through all typed nodes" $ liftEffect $ do
            pure unit

        pending' "it is possible to iterate through all raw nodes" $ liftEffect $ do
            pure unit

    describe "iterating through links" $ do

        pending' "it is possible to iterate through all links in the patch" $ liftEffect $ do
            pure unit