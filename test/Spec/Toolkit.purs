module Test.Spec.Toolkit where

import Prelude

import Test.Spec (Spec, pending, describe, it, pending', itOnly)
import Test.Spec.Assertions (fail, shouldEqual)


import Noodle.Toolkit (Toolkit)


spec :: Spec Unit
spec = do

    describe "registering" $ do

        it "registers family" $ do
            pure unit


    describe "spawning" $ do

        it "spawning node from a family" $ do
            pure unit


    describe "registering & spawning" $ do

        it "it is possible to register family and immediately spawn the node that belongs to it" $ do
            pure unit