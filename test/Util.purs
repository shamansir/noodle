module RpdTest.Util
    ( runWith
    , withRpd
    , spec
    ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

import Data.String as String

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)

import Rpd.API (Rpd) as Rpd
import Rpd.Network (Network) as Rpd
import Rpd.Network (empty) as Network
import Rpd.Log (runRpdLogging) as RL
import Rpd.UUID as UUID


runWith :: forall d. Rpd.Network d -> (Rpd.Network d -> Aff Unit) -> Aff Unit
runWith initialNetwork f = f initialNetwork


-- runWith :: forall e d. Rpd.Network d -> (Rpd.Network d -> TestAff e) -> TestAff e
-- runWith initialNetwork f = do
--   newNetwork <- liftEff $ do
--     networkRef <- newRef Rpd.empty
--     Rpd.run (writeRef networkRef) initialNetwork
--     readRef networkRef
--   f newNetwork


withRpd
  :: forall d
   . (Rpd.Network d -> Aff Unit)
  -> Rpd.Rpd (Rpd.Network d)
  -> Aff Unit
withRpd test rpd =
  liftEffect (getNetwork rpd) >>= test
  where
    --getNetwork :: R.Rpd d e -> R.RpdEff e (R.Network d e)
    getNetwork rpd' = do
      nwTarget <- Ref.new $ Network.empty "foo"
      _ <- RL.runRpdLogging (flip Ref.write $ nwTarget) rpd'
      Ref.read nwTarget


spec :: Spec Unit
spec =
  describe "UUID generation" do
    it "does what it says" do
      uuid <- liftEffect $ UUID.new
      _ <- 36 `shouldEqual` String.length uuid
      pure unit
