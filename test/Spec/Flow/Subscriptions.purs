module Rpd.Test.Spec.Flow.Subscriptions
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

import Rpd.API
    ( NodeInletsSubscription, NodeOutletsSubscription
    , InletSubscription
    ) as R
import Rpd.API.Action as R
import Rpd.API.Action.Sequence ((</>))
import Rpd.API.Action.Sequence (init, run, runFolding, pushAll) as Actions
import Rpd.API.Action.Sequence as R
import Rpd.Process (InletHandler(..), InletAlias, OutletAlias) as R
import Rpd.Path (toPatch, toNode, toInlet)
import Rpd.UUID as UUID
import Rpd.Util (flow) as R
import Rpd.Network (empty) as Network
import Rpd.Network (Network) as R

import Test.Spec (Spec, it, describe, pending, pending', itOnly)
import Test.Spec.Assertions (shouldEqual, shouldContain, shouldNotContain)

import Rpd.Test.Util.Either (getOrFail)
import Rpd.Test.Util.Spy as Spy
import Rpd.Test.Spec.Flow.Base (Delivery(..), Pipe(..), Node(..), Actions, myToolkit)


{- ======================================= -}
{- ============ SUBSCRIPTIONS ============ -}
{- ======================================= -}


spec :: Spec Unit
spec = do

  describe "subscribing node" $ do

    it "subscribing to node passes the node data to the inlet subscriber" $ do
      traceSpy <- liftEffect Spy.trace

      let
        -- inletHandler :: R.InletAlias -> UUID.ToInlet -> Delivery -> Effect Unit
        inletHandler :: R.NodeInletsSubscription Delivery
        inletHandler alias _ v = Spy.consider traceSpy $ alias /\ v
          -- Ref.modify ((:) $ alias /\ v) ref >>= pure <<< const unit
        -- outletHandler :: R.OutletAlias -> UUID.ToOutlet -> Delivery -> Effect Unit
        outletHandler :: R.NodeOutletsSubscription Delivery
        outletHandler alias _ v = pure unit
        structure :: Actions
        structure =
          Actions.init
            </> R.addPatch "patch"
            </> R.addNode (toPatch "patch") "node" Empty
            </> R.addInlet (toNode "patch" "node") "inlet" Pass

      _ <- liftEffect
        $ Actions.runFolding
            myToolkit
            (Network.empty "network")
            $ structure
                 </> R.subscribeToNode (toNode "patch" "node") inletHandler outletHandler
                 </> R.streamToInlet
                        (toInlet "patch" "node" "inlet")
                        (R.flow $ const Notebook <$> interval 30)

                --  </> R.do_ $ \nw -> do
                --         _ <- launchAff_ $ delay (Milliseconds 100.0)

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
        $ Actions.runFolding
            myToolkit
            (Network.empty "network")
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
        $ Actions.runFolding
            myToolkit
            network
            $ structure
                 </> R.subscribeToInlet (toInlet "patch" "node" "inlet") (Spy.with traceSpy)
                 </> R.streamToInlet
                        (toInlet "patch" "node" "inlet")
                        (R.flow $ const Notebook <$> interval 30)
      network' <- getOrFail result network

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
        $ Actions.runFolding
            myToolkit
            network
            $ structure
                 </> R.subscribeToInlet (toInlet "patch" "node" "inlet") (Spy.with traceSpy)
                 </> R.streamToInlet
                        (toInlet "patch" "node" "inlet")
                        (R.flow $ const Notebook <$> interval 30)
      network' <- getOrFail result network

      delay (Milliseconds 100.0)
      _ <- liftEffect stop
      vals <- liftEffect $ Spy.get traceSpy
      vals `shouldContain` Notebook
      liftEffect $ Spy.reset traceSpy

      result' /\ { stop : stop' } <- liftEffect
        $ Actions.runFolding
            myToolkit
            network'
            $ Actions.init
                 </> R.removeInlet (toInlet "patch" "node" "inlet")
                 </> R.addInlet (toNode "patch" "node") "inlet" Pass
                 </> R.streamToInlet
                        (toInlet "patch" "node" "inlet")
                        (R.flow $ const Liver <$> interval 30)
      _ <- getOrFail result' network'

      delay (Milliseconds 100.0)
      _ <- liftEffect stop
      vals' <- liftEffect $ Spy.get traceSpy
      vals' `shouldNotContain` Liver
      vals' `shouldNotContain` Notebook
      vals' `shouldEqual` []

      pure unit


  describe "subscribing outlet" $ do

    pending "subscribing to inlet passes the inlet data to the subscriber"

    pending "when the outlet was removed after the subscription, the subscriber stops receiving data"

    pending "when the outlet was removed and again added after the subscription, the subscriber still doesn't receive anything"
