module RpdTest.Flow.Subscriptions
    ( spec
    ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Array ((:))
import Data.Tuple.Nested ((/\), type (/\))

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Aff (delay, launchAff_) --, throwError

import FRP.Event.Time (interval)

import Rpd.API.Action as R
import Rpd.API.Action.Sequence ((</>))
import Rpd.API.Action.Sequence (init, run, runFolding) as Actions
import Rpd.API.Action.Sequence as R
import Rpd.Process (InletHandler(..), InletAlias, OutletAlias) as R
import Rpd.Path (toPatch, toNode, toInlet)
import Rpd.UUID as UUID
import Rpd.Util (flow) as R
import Rpd.Network (empty) as Network
import Rpd.Network (Network) as R

import Test.Spec (Spec, it, describe, pending, pending')
import Test.Spec.Assertions (shouldEqual, shouldContain, shouldNotContain)

import RpdTest.Helper (getOrFail)
import RpdTest.Flow.Base (Delivery(..), Pipe(..), Node(..), Actions, myToolkit)


{- ======================================= -}
{- ============ SUBSCRIPTIONS ============ -}
{- ======================================= -}


spec :: Spec Unit
spec = do

  describe "subscribing node" $ do

    it "subscribing to node passes the node data to the inlet subscriber" $ do
      ref <- liftEffect $ Ref.new []

      let
        -- inletHandler :: R.InletAlias -> UUID.ToInlet -> Delivery -> Effect Unit
        inletHandler :: R.NodeInletsSubscription Delivery
        inletHandler alias _ v =
          Ref.modify ((:) $ alias /\ v) ref >>= pure <<< const unit
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
      vals <- liftEffect $ Ref.read ref
      vals `shouldContain` ("inlet" /\ Notebook)

      pure unit

    pending "when the node was removed after the subscription, the subscriber stops receiving data"

    pending "when the node was removed and again added after the subscription, the subscriber still doesn't receive anything"

  describe "subscribing inlet" $ do

    it "subscribing to inlet passes the inlet data to the subscriber" $ do
      ref <- liftEffect $ Ref.new []
      let
        handler :: R.InletSubscription Delivery
        handler = \v ->
          Ref.modify ((:) v) ref >>= pure <<< const unit
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
                 </> R.subscribeToInlet (toInlet "patch" "node" "inlet") handler
                 </> R.streamToInlet
                        (toInlet "patch" "node" "inlet")
                        (R.flow $ const Notebook <$> interval 30)

      delay (Milliseconds 100.0)
      vals <- liftEffect $ Ref.read ref
      vals `shouldContain` Notebook

      pure unit

    -- TODO: test values come in order they were sent (i.e. send folded stream with IDs or
    --       stream different values after a delay)

    it "when the inlet was removed after the subscription, the subscriber stops receiving data" $ do
      ref <- liftEffect $ Ref.new []

      let
        handler :: R.InletSubscription Delivery
        handler = \v ->
          Ref.modify ((:) v) ref >>= pure <<< const unit
        structure :: Actions
        structure =
          Actions.init
            </> R.addPatch "patch"
            </> R.addNode (toPatch "patch") "node" Empty
            </> R.addInlet (toNode "patch" "node") "inlet" Pass
        network :: R.Network Delivery Pipe Node
        network = Network.empty "network"

      result <- liftEffect
        $ Actions.runFolding
            myToolkit
            network
            $ structure
                 </> R.subscribeToInlet (toInlet "patch" "node" "inlet") handler
                 </> R.streamToInlet
                        (toInlet "patch" "node" "inlet")
                        (R.flow $ const Notebook <$> interval 30)
      network' <- getOrFail result network

      delay (Milliseconds 100.0)
      vals <- liftEffect $ Ref.read ref
      vals `shouldContain` Notebook
      liftEffect $ Ref.write [] ref

      result' <- liftEffect
        $ Actions.runFolding
            myToolkit
            network'
            $ Actions.init
                 </> R.removeInlet (toInlet "patch" "node" "inlet")
                 </> R.streamToInlet
                        (toInlet "patch" "node" "inlet")
                        (R.flow $ const Liver <$> interval 30)
      _ <- getOrFail result' network'

      delay (Milliseconds 100.0)
      vals' <- liftEffect $ Ref.read ref
      vals' `shouldNotContain` Liver
      vals' `shouldNotContain` Notebook
      vals' `shouldEqual` []
      liftEffect $ Ref.write [] ref

      pure unit

    it "when the inlet was removed and again added after the subscription, the subscriber still doesn't receive anything" $ do
      ref <- liftEffect $ Ref.new []

      let
        handler :: R.InletSubscription Delivery
        handler = \v ->
          Ref.modify ((:) v) ref >>= pure <<< const unit
        structure :: Actions
        structure =
          Actions.init
            </> R.addPatch "patch"
            </> R.addNode (toPatch "patch") "node" Empty
            </> R.addInlet (toNode "patch" "node") "inlet" Pass
        network :: R.Network Delivery Pipe Node
        network = Network.empty "network"

      result <- liftEffect
        $ Actions.runFolding
            myToolkit
            network
            $ structure
                 </> R.subscribeToInlet (toInlet "patch" "node" "inlet") handler
                 </> R.streamToInlet
                        (toInlet "patch" "node" "inlet")
                        (R.flow $ const Notebook <$> interval 30)
      network' <- getOrFail result network

      delay (Milliseconds 100.0)
      vals <- liftEffect $ Ref.read ref
      vals `shouldContain` Notebook
      liftEffect $ Ref.write [] ref

      result' <- liftEffect
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
      vals' <- liftEffect $ Ref.read ref
      vals' `shouldNotContain` Liver
      vals' `shouldNotContain` Notebook
      vals' `shouldEqual` []
      liftEffect $ Ref.write [] ref

      pure unit


  describe "subscribing outlet" $ do

    pending "subscribing to inlet passes the inlet data to the subscriber"

    pending "when the outlet was removed after the subscription, the subscriber stops receiving data"

    pending "when the outlet was removed and again added after the subscription, the subscriber still doesn't receive anything"
