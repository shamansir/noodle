module Noodle.Test.Spec.Flow.Subpatches
    ( spec
    ) where


import Prelude

import Effect.Class (liftEffect)

import FSM (run) as Actions

import Noodle.API.Action.Sequence ((</>))
import Noodle.API.Action.Sequence (init, pushAll) as Actions
import Noodle.API.Action.Sequence as R
import Noodle.Path (toPatch, toNode, toInlet, toOutlet)
import Noodle.Network (empty) as Network

import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

import Noodle.Test.Util.Spy as Spy
import Noodle.Test.Spec.Flow.Base (Delivery(..), Pipe(..), Node(..), Actions, mySequencer)

spec :: Spec Unit
spec = do

  describe "supbatches" $ do

    it "connections between patches work" $ do
        lastSpy <- liftEffect Spy.last
        { push } <- liftEffect
            $ Actions.run
                mySequencer
                (pure $ Network.empty "network")

        liftEffect
                $  Actions.pushAll push
                $  Actions.init
                </> R.addPatch "src-patch"
                </> R.addNode (toPatch "src-patch") "node" Empty
                </> R.addOutlet (toNode "src-patch" "node") "outlet" Pass
                </> R.addPatch "dst-patch"
                </> R.addNode (toPatch "dst-patch") "node" Empty
                </> R.addInlet (toNode "dst-patch" "node") "inlet" Pass
                </> R.connect
                        (toOutlet "src-patch" "node" "outlet")
                        (toInlet "dst-patch" "node" "inlet")
                </> R.subscribeToInlet (toInlet "dst-patch" "node" "inlet") (Spy.with lastSpy)
                </> R.sendToOutlet
                    (toOutlet "src-patch" "node" "outlet")
                    Notebook

        lastValAtInlet <- liftEffect $ Spy.get lastSpy
        lastValAtInlet `shouldEqual` (pure Notebook)

        pure unit

    it "subpatch is just the node, proxying the flow to the other patch" $ do
        lastSpy <- liftEffect Spy.last
        { push } <- liftEffect
            $ Actions.run
                mySequencer
                (pure $ Network.empty "network")

        liftEffect
                $  Actions.pushAll push
                $  Actions.init
                </> R.addPatch "src-patch"
                </> R.addNode (toPatch "src-patch") "node" Empty
                </> R.addOutlet (toNode "src-patch" "node") "outlet" Pass
                </> R.addPatch "dst-patch"
                </> R.addNode (toPatch "dst-patch") "node" Empty
                </> R.addInlet (toNode "dst-patch" "node") "inlet" Pass
                </> R.addNode (toPatch "src-patch") "subp-node" Empty
                </> R.addInlet (toNode "src-patch" "subp-node") "inlet" Pass
                </> R.subscribeToInlet
                        (toInlet "src-patch" "subp-node" "inlet")
                        (push <<< R.sendToInlet (toInlet "dst-patch" "node" "inlet"))
                </> R.subscribeToInlet
                        (toInlet "dst-patch" "node" "inlet")
                        (Spy.with lastSpy)
                </> R.connect
                        (toOutlet "src-patch" "node" "outlet")
                        (toInlet "src-patch" "subp-node" "inlet")
                </> R.sendToOutlet
                    (toOutlet "src-patch" "node" "outlet")
                    Notebook

        lastValAtInlet <- liftEffect $ Spy.get lastSpy
        lastValAtInlet `shouldEqual` (pure Notebook)

        pure unit
