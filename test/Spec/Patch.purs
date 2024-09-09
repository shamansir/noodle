module Test.Spec.Patch where

import Prelude

import Effect.Class (liftEffect)

import Data.Tuple.Nested ((/\))

import Test.Spec (Spec, pending, describe, it, pending', itOnly)
import Test.Spec.Assertions (fail, shouldEqual)


import Noodle.Node (run, _listenUpdatesAndRun, state, modifyState) as Node
import Noodle.Node ((#->), (@->), (<=@))
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (empty, spawn, register) as Toolkit
import Noodle.Toolkit.Families (Families, F, FNil, type (//))

import Test.MyToolkit.Node.Concat as Concat
import Test.MyToolkit.Node.Sum as Sum
import Test.MyToolkit.Node.Stateful as Stateful
import Test.MyToolkit.Toolkit (Toolkit, toolkit) as My


spec :: Spec Unit
spec = do

    describe "registering nodes inside the patch" $ do

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