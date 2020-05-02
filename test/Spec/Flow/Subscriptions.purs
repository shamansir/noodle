module Noodle.Test.Spec.Flow.Subscriptions
    ( spec
    ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Array ((:), snoc)
import Data.Tuple (curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Aff (delay, launchAff_) --, throwError

import FRP.Event.Time (interval)

import FSM (fold, run) as Actions

import Noodle.API
    ( NodeInletsSubscription, NodeOutletsSubscription
    , InletSubscription
    ) as R
import Noodle.API.Action as R
import Noodle.API.Action.Sequence ((</>))
import Noodle.API.Action.Sequence (init, pushAll) as Actions
import Noodle.API.Action.Sequence as R
import Noodle.Process (InletHandler(..), InletAlias, OutletAlias) as R
import Noodle.Path (toPatch, toNode, toInlet, toOutlet)
import Noodle.UUID as UUID
import Noodle.Util (flow) as R
import Noodle.Network (empty) as Network
import Noodle.Network (Network) as R

import Test.Spec (Spec, it, describe, pending, pending', itOnly)
import Test.Spec.Assertions (shouldEqual, shouldContain, shouldNotContain)

import Noodle.Test.Util.Actions (getOrFail, getOrFail')
import Noodle.Test.Util.Spy as Spy
import Noodle.Test.Spec.Flow.Base (Delivery(..), Pipe(..), Node(..), Actions, mySequencer)


{- ======================================= -}
{- ============ SUBSCRIPTIONS ============ -}
{- ======================================= -}


spec :: Spec Unit
spec = do

  describe "subscribing node" $ do

    it "subscribing to node passes the node data to the inlet subscriber" $ do
      traceSpy <- liftEffect Spy.trace

      let
        inletHandler :: R.NodeInletsSubscription Delivery
        inletHandler alias _ v = Spy.consider traceSpy $ alias /\ v
        outletHandler :: R.NodeOutletsSubscription Delivery
        outletHandler alias _ v = pure unit
        structure :: Actions
        structure =
          Actions.init
            </> R.addPatch "patch"
            </> R.addNode (toPatch "patch") "node" Empty
            </> R.addInlet (toNode "patch" "node") "inlet" Pass

      _ <- liftEffect
        $ Actions.fold
            mySequencer
            (pure $ Network.empty "network")
            $ structure
                 </> R.subscribeToNode (toNode "patch" "node") inletHandler outletHandler
                 </> R.streamToInlet
                        (toInlet "patch" "node" "inlet")
                        (R.flow $ const Notebook <$> interval 30)

      delay (Milliseconds 100.0)
      vals <- liftEffect $ Spy.get traceSpy
      vals `shouldContain` ("inlet" /\ Notebook)

      pure unit

    pending "when the node was removed after the subscription, the subscriber stops receiving data"

    pending "when the node was removed and again added after the subscription, the subscriber still doesn't receive anything"

  describe "subscribing inlet" $ do

    it "subscribing to inlet passes the inlet data to the subscriber" $ do
      traceSpy <- liftEffect Spy.trace
      let
        structure :: Actions
        structure =
          Actions.init
            </> R.addPatch "patch"
            </> R.addNode (toPatch "patch") "node" Empty
            </> R.addInlet (toNode "patch" "node") "inlet" Pass

      _ <- liftEffect
        $ Actions.fold
            mySequencer
            (pure $ Network.empty "network")
            $ structure
                 </> R.subscribeToInlet (toInlet "patch" "node" "inlet") (Spy.with traceSpy)
                 </> R.streamToInlet
                        (toInlet "patch" "node" "inlet")
                        (R.flow $ const Notebook <$> interval 30)

      delay (Milliseconds 100.0)
      vals <- liftEffect $ Spy.get traceSpy
      vals `shouldContain` Notebook

      pure unit

    -- TODO: test values come in order they were sent (i.e. send folded stream with IDs or
    --       stream different values after a delay)

    it "when the inlet was removed after the subscription, the subscriber stops receiving data" $ do
      traceSpy <- liftEffect Spy.trace

      let
        structure :: Actions
        structure =
          Actions.init
            </> R.addPatch "patch"
            </> R.addNode (toPatch "patch") "node" Empty
            </> R.addInlet (toNode "patch" "node") "inlet" Pass
        network :: R.Network Delivery Pipe Node
        network = Network.empty "network"

      result /\ { pushAction, stop } <- liftEffect
        $ Actions.fold
            mySequencer
            (pure network)
            $ structure
                 </> R.subscribeToInlet (toInlet "patch" "node" "inlet") (Spy.with traceSpy)
                 </> R.streamToInlet
                        (toInlet "patch" "node" "inlet")
                        (R.flow $ const Notebook <$> interval 30)
      network' <- getOrFail result

      delay (Milliseconds 100.0)
      vals <- liftEffect $ Spy.get traceSpy
      vals `shouldContain` Notebook
      liftEffect $ Spy.reset traceSpy

      _ <- liftEffect
        $ Actions.pushAll pushAction
        $ Actions.init
            </> R.removeInlet (toInlet "patch" "node" "inlet")
            </> R.streamToInlet
                  (toInlet "patch" "node" "inlet")
                  (R.flow $ const Liver <$> interval 30)

      delay (Milliseconds 100.0)
      vals' <- liftEffect $ Spy.get traceSpy
      vals' `shouldNotContain` Liver
      vals' `shouldNotContain` Notebook
      vals' `shouldEqual` []

      liftEffect stop

    it "when the inlet was removed and again added after the subscription, the subscriber still doesn't receive anything" $ do
      traceSpy <- liftEffect Spy.trace

      let
        structure :: Actions
        structure =
          Actions.init
            </> R.addPatch "patch"
            </> R.addNode (toPatch "patch") "node" Empty
            </> R.addInlet (toNode "patch" "node") "inlet" Pass
        network :: R.Network Delivery Pipe Node
        network = Network.empty "network"

      result /\ { stop } <- liftEffect
        $ Actions.fold
            mySequencer
            (pure network)
            $ structure
                 </> R.subscribeToInlet (toInlet "patch" "node" "inlet") (Spy.with traceSpy)
                 </> R.streamToInlet
                        (toInlet "patch" "node" "inlet")
                        (R.flow $ const Notebook <$> interval 30)
      network' <- getOrFail result

      delay (Milliseconds 100.0)
      _ <- liftEffect stop
      vals <- liftEffect $ Spy.get traceSpy
      vals `shouldContain` Notebook
      liftEffect $ Spy.reset traceSpy

      result' /\ { stop : stop' } <- liftEffect
        $ Actions.fold
            mySequencer
            (pure network')
            $ Actions.init
                 </> R.removeInlet (toInlet "patch" "node" "inlet")
                 </> R.addInlet (toNode "patch" "node") "inlet" Pass
                 </> R.streamToInlet
                        (toInlet "patch" "node" "inlet")
                        (R.flow $ const Liver <$> interval 30)
      _ <- getOrFail result

      delay (Milliseconds 100.0)
      _ <- liftEffect stop
      vals' <- liftEffect $ Spy.get traceSpy
      vals' `shouldNotContain` Liver
      vals' `shouldNotContain` Notebook
      vals' `shouldEqual` []

      pure unit

    pending "removing the inlet is only affecting the processing by not having a corresponding value"


  describe "subscribing outlet" $ do

    it "subscribing to outlet passes the outlet data to the subscriber" do
      traceSpy <- liftEffect Spy.trace
      let
        structure :: Actions
        structure =
          Actions.init
            </> R.addPatch "patch"
            </> R.addNode (toPatch "patch") "node" Empty
            </> R.addOutlet (toNode "patch" "node") "outlet" Pass

      _ <- liftEffect
        $ Actions.fold
            mySequencer
            (pure $ Network.empty "network")
            $ structure
                 </> R.subscribeToOutlet (toOutlet "patch" "node" "outlet") (Spy.with traceSpy)
                 </> R.streamToOutlet
                        (toOutlet "patch" "node" "outlet")
                        (R.flow $ const Notebook <$> interval 30)

      delay (Milliseconds 100.0)
      vals <- liftEffect $ Spy.get traceSpy
      vals `shouldContain` Notebook

      pure unit

    it "when the outlet was removed after the subscription, the subscriber stops receiving data" do
      traceSpy <- liftEffect Spy.trace

      let
        structure :: Actions
        structure =
          Actions.init
            </> R.addPatch "patch"
            </> R.addNode (toPatch "patch") "node" Empty
            </> R.addOutlet (toNode "patch" "node") "outlet" Pass
        network :: R.Network Delivery Pipe Node
        network = Network.empty "network"

      result /\ { pushAction, stop } <- liftEffect
        $ Actions.fold
            mySequencer
            (pure network)
            $ structure
                 </> R.subscribeToOutlet (toOutlet "patch" "node" "outlet") (Spy.with traceSpy)
                 </> R.streamToOutlet
                        (toOutlet "patch" "node" "outlet")
                        (R.flow $ const Notebook <$> interval 30)
      network' <- getOrFail result

      delay (Milliseconds 100.0)
      vals <- liftEffect $ Spy.get traceSpy
      vals `shouldContain` Notebook
      liftEffect $ Spy.reset traceSpy

      _ <- liftEffect
        $ Actions.pushAll pushAction
        $ Actions.init
            </> R.removeOutlet (toOutlet "patch" "node" "outlet")
            </> R.streamToOutlet
                  (toOutlet "patch" "node" "outlet")
                  (R.flow $ const Liver <$> interval 30)

      delay (Milliseconds 100.0)
      vals' <- liftEffect $ Spy.get traceSpy
      vals' `shouldNotContain` Liver
      vals' `shouldNotContain` Notebook
      vals' `shouldEqual` []

      liftEffect stop

    it "when the outlet was removed and again added after the subscription, the subscriber still doesn't receive anything" do
      traceSpy <- liftEffect Spy.trace

      let
        structure :: Actions
        structure =
          Actions.init
            </> R.addPatch "patch"
            </> R.addNode (toPatch "patch") "node" Empty
            </> R.addOutlet (toNode "patch" "node") "outlet" Pass
        network :: R.Network Delivery Pipe Node
        network = Network.empty "network"

      result /\ { stop } <- liftEffect
        $ Actions.fold
            mySequencer
            (pure network)
            $ structure
                 </> R.subscribeToOutlet (toOutlet "patch" "node" "outlet") (Spy.with traceSpy)
                 </> R.streamToOutlet
                        (toOutlet "patch" "node" "outlet")
                        (R.flow $ const Notebook <$> interval 30)
      network' <- getOrFail result

      delay (Milliseconds 100.0)
      _ <- liftEffect stop
      vals <- liftEffect $ Spy.get traceSpy
      vals `shouldContain` Notebook
      liftEffect $ Spy.reset traceSpy

      result' /\ { stop : stop' } <- liftEffect
        $ Actions.fold
            mySequencer
            (pure network')
            $ Actions.init
                 </> R.removeOutlet (toOutlet "patch" "node" "outlet")
                 </> R.addOutlet (toNode "patch" "node") "outlet" Pass
                 </> R.streamToOutlet
                        (toOutlet "patch" "node" "outlet")
                        (R.flow $ const Liver <$> interval 30)
      _ <- getOrFail result

      delay (Milliseconds 100.0)
      _ <- liftEffect stop
      vals' <- liftEffect $ Spy.get traceSpy
      vals' `shouldNotContain` Liver
      vals' `shouldNotContain` Notebook
      vals' `shouldEqual` []

      pure unit
