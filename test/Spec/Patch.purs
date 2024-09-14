module Test.Spec.Patch where

import Prelude

import Effect.Class (liftEffect)

import Test.Spec (Spec, pending, describe, it, pending', itOnly)
import Test.Spec.Assertions (fail, shouldEqual)

import Noodle.Patch (make, registerNodeNotFromToolkit, registerNode, registerRawNode, mapAllNodes) as Patch
import Noodle.Raw.Node (family) as Node

import Test.MyToolkit.Node.Concat as Concat


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
                    Patch.mapAllNodes Node.family patchWithNodes
            (show <$> nodesInPatch) `shouldEqual` [ "concat" ]


        pending' "registering a node from toolkit by family" $ liftEffect $ do
            pure unit


        pending' "registering a raw node from toolkit by family" $ liftEffect $ do
            pure unit

    describe "connecting & disconnecting nodes inside the patch" $ do

        pending' "it is possible to connect nodes inside the patch" $ liftEffect $ do
            pure unit

        pending' "it is possible to connect and then disconnect nodes inside the patch" $ liftEffect $ do
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