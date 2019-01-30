module RpdTest.Flow.Subscriptions
    ( spec
    ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Array ((:))

import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Aff (delay) --, throwError

import FRP.Event.Time (interval)

import Rpd (run) as R
import Rpd.API ((</>))
import Rpd.API as R
import Rpd.Process (InletHandler(..)) as R
import Rpd.Path (inletPath, nodePath, patchId)
import Rpd.Util (flow) as R

import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual, shouldContain, shouldNotContain)

import RpdTest.Util (withRpd)
import RpdTest.Flow.Base (MyRpd, Delivery(..))


{- ======================================= -}
{- ============ SUBSCRIPTIONS ============ -}
{- ======================================= -}


spec :: Spec Unit
spec = do
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
          </> R.addNode (patchId 0) "node"
          </> R.addInlet (nodePath 0 0) "inlet"
          </> R.subscribeInlet (inletPath 0 0 0) handler

    rpd # withRpd \nw -> do
      _ <- liftEffect
              $ R.run (const unit) (const unit)
              $ nw # R.streamToInlet
                (inletPath 0 0 0)
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
          </> R.addNode (patchId 0) "node"
          </> R.addInlet (nodePath 0 0) "inlet"
          </> R.subscribeInlet (inletPath 0 0 0) handler

    rpd # withRpd \nw -> do
      _ <- liftEffect
              $ R.run (const unit) (const unit) -- FIXME: report the error as `Aff.throwError`
              $ nw # R.streamToInlet
                        (inletPath 0 0 0)
                        (R.flow $ const Notebook <$> interval 30)
      delay (Milliseconds 100.0)
      vals <- liftEffect $ Ref.read ref
      vals `shouldContain` Notebook
      _ <- liftEffect $ Ref.write [] ref
      vals' <- liftEffect $ Ref.read ref
      _ <- liftEffect
              $ R.run (const unit) (const unit)
              $ nw  #  R.removeInlet (inletPath 0 0 0)
                   </> R.streamToInlet
                          (inletPath 0 0 0)
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
          </> R.addNode (patchId 0) "node"
          </> R.addInlet (nodePath 0 0) "inlet"
          </> R.subscribeInlet (inletPath 0 0 0) handler

    rpd # withRpd \nw -> do
      _ <- liftEffect
              $ R.run (const unit) (const unit) -- FIXME: report the error as `Aff.throwError`
              $ nw # R.streamToInlet
                        (inletPath 0 0 0)
                        (R.flow $ const Notebook <$> interval 30)
      delay (Milliseconds 100.0)
      vals <- liftEffect $ Ref.read ref
      vals `shouldContain` Notebook
      _ <- liftEffect $ Ref.write [] ref
      vals' <- liftEffect $ Ref.read ref
      _ <- liftEffect
              $ R.run (const unit) (const unit)
              $ nw  #  R.removeInlet (inletPath 0 0 0)
                   </> R.addInlet (nodePath 0 0) "inlet" -- notice the inlet is added back
                   </> R.streamToInlet
                          (inletPath 0 0 0)
                          (R.flow $ const Liver <$> interval 30)
      delay (Milliseconds 100.0)
      vals'' <- liftEffect $ Ref.read ref
      vals'' `shouldNotContain` Liver
      vals'' `shouldEqual` []
      pure unit

    pure unit
