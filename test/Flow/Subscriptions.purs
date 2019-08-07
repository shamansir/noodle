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
import Effect.Aff (delay) --, throwError

import FRP.Event.Time (interval)

import Rpd.API ((</>))
import Rpd.API as R
import Rpd.Process (InletHandler(..), InletAlias, OutletAlias) as R
import Rpd.Path (toPatch, toNode, toInlet)
import Rpd.UUID as UUID
import Rpd.Util (flow) as R

import Test.Spec (Spec, it, describe, pending)
import Test.Spec.Assertions (shouldEqual, shouldContain, shouldNotContain)

import RpdTest.Util (withRpd)
import RpdTest.Flow.Base (MyRpd, Delivery(..), Pipe(..), Node(..))


{- ======================================= -}
{- ============ SUBSCRIPTIONS ============ -}
{- ======================================= -}


spec :: Spec Unit
spec = do

  describe "subscribing node" $ do

    it "subscribing to node passes the node data to the inlet subscriber" $ do
      ref <- liftEffect $ Ref.new []
      let
        inletHandler :: (R.InletAlias /\ UUID.ToInlet /\ Delivery) -> Effect Unit
        inletHandler (alias /\ _ /\ v) =
          Ref.modify ((:) $ alias /\ v) ref >>= pure <<< const unit
        outletHandler :: (R.OutletAlias /\ UUID.ToOutlet /\ Delivery) -> Effect Unit
        outletHandler (alias /\ _ /\ v) = pure unit
        rpd :: MyRpd
        rpd =
          R.init "network"
            </> R.addPatch "patch"
            </> R.addNode (toPatch "patch") "node" Empty
            </> R.addInlet (toNode "patch" "node") "inlet" Pass
            </> R.subscribeNode (toNode "patch" "node") inletHandler outletHandler
      rpd # withRpd \nw -> do
        _ <- liftEffect
                $ R.run (const unit) (const unit)
                $ nw # R.streamToInlet
                    (toInlet "patch" "node" "inlet")
                    (R.flow $ const Notebook <$> interval 30)
        delay (Milliseconds 100.0)
        vals <- liftEffect $ Ref.read ref
        vals `shouldContain` ("inlet" /\ Notebook)
        pure unit

      pure unit

    pending "when the node was removed after the subscription, the subscriber stops receiving data"

    pending "when the node was removed and again added after the subscription, the subscriber still doesn't receive anything"

  describe "subscribing inlet" $ do

    it "subscribing to inlet passes the inlet data to the subscriber" $ do
      ref <- liftEffect $ Ref.new []
      let
        handler :: R.InletHandler Delivery
        handler = R.InletHandler $ \v ->
          Ref.modify ((:) v) ref >>= pure <<< const unit
        rpd :: MyRpd
        rpd =
          R.init "network"
            </> R.addPatch "patch"
            </> R.addNode (toPatch "patch") "node" Empty
            </> R.addInlet (toNode "patch" "node") "inlet" Pass
            </> R.subscribeInlet (toInlet "patch" "node" "inlet") handler

      rpd # withRpd \nw -> do
        _ <- liftEffect
                $ R.run (const unit) (const unit)
                $ nw # R.streamToInlet
                  (toInlet "patch" "node" "inlet")
                  (R.flow $ const Notebook <$> interval 30)
        delay (Milliseconds 100.0)
        vals <- liftEffect $ Ref.read ref
        vals `shouldContain` Notebook
        pure unit

      pure unit

    -- TODO: test values come in order they were sent (i.e. send folded stream with IDs or
    --       stream different values after a delay)

    it "when the inlet was removed after the subscription, the subscriber stops receiving data" $ do
      ref <- liftEffect $ Ref.new []
      let
        handler :: R.InletHandler Delivery
        handler = R.InletHandler $ \v ->
          Ref.modify ((:) v) ref >>= pure <<< const unit
        rpd :: MyRpd
        rpd =
          R.init "network"
            </> R.addPatch "patch"
            </> R.addNode (toPatch "patch") "node" Empty
            </> R.addInlet (toNode "patch" "node") "inlet" Pass
            </> R.subscribeInlet (toInlet "patch" "node" "inlet") handler

      rpd # withRpd \nw -> do
        _ <- liftEffect
                $ R.run (const unit) (const unit) -- FIXME: report the error as `Aff.throwError`
                $ nw # R.streamToInlet
                          (toInlet "patch" "node" "inlet")
                          (R.flow $ const Notebook <$> interval 30)
        delay (Milliseconds 100.0)
        vals <- liftEffect $ Ref.read ref
        vals `shouldContain` Notebook
        _ <- liftEffect $ Ref.write [] ref
        vals' <- liftEffect $ Ref.read ref
        _ <- liftEffect
                $ R.run (const unit) (const unit)
                $ nw  #  R.removeInlet (toInlet "patch" "node" "inlet")
                    </> R.streamToInlet
                            (toInlet "patch" "node" "inlet")
                            (R.flow $ const Liver <$> interval 30)
        delay (Milliseconds 100.0)
        vals'' <- liftEffect $ Ref.read ref
        vals'' `shouldNotContain` Liver
        vals'' `shouldEqual` []
        pure unit

    it "when the inlet was removed and again added after the subscription, the subscriber still doesn't receive anything" $ do
      ref <- liftEffect $ Ref.new []
      let
        handler :: R.InletHandler Delivery
        handler = R.InletHandler $ \v ->
          Ref.modify ((:) v) ref >>= pure <<< const unit
        rpd :: MyRpd
        rpd =
          R.init "network"
            </> R.addPatch "patch"
            </> R.addNode (toPatch "patch") "node" Empty
            </> R.addInlet (toNode "patch" "node") "inlet" Pass
            </> R.subscribeInlet (toInlet "patch" "node" "inlet") handler

      rpd # withRpd \nw -> do
        _ <- liftEffect
                $ R.run (const unit) (const unit) -- FIXME: report the error as `Aff.throwError`
                $ nw # R.streamToInlet
                          (toInlet "patch" "node" "inlet")
                          (R.flow $ const Notebook <$> interval 30)
        delay (Milliseconds 100.0)
        vals <- liftEffect $ Ref.read ref
        vals `shouldContain` Notebook
        _ <- liftEffect $ Ref.write [] ref
        vals' <- liftEffect $ Ref.read ref
        _ <- liftEffect
                $ R.run (const unit) (const unit)
                $ nw  #  R.removeInlet (toInlet "patch" "node" "inlet")
                    </> R.addInlet (toNode "patch" "node") "inlet" Pass
                      -- notice the inlet is added back
                    </> R.streamToInlet
                            (toInlet "patch" "node" "inlet")
                            (R.flow $ const Liver <$> interval 30)
        delay (Milliseconds 100.0)
        vals'' <- liftEffect $ Ref.read ref
        vals'' `shouldNotContain` Liver
        vals'' `shouldEqual` []
        pure unit

      pure unit

  describe "subscribing outlet" $ do

    pending "subscribing to inlet passes the inlet data to the subscriber"

    pending "when the outlet was removed after the subscription, the subscriber stops receiving data"

    pending "when the outlet was removed and again added after the subscription, the subscriber still doesn't receive anything"
