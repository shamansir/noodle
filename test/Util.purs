module RpdTest.Util
    ( runWith
    , withRpd
    ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

import Rpd.API (Rpd) as Rpd
import Rpd.Network (Network) as Rpd
import Rpd.Network (empty) as Network
import Rpd.Log (runRpdLogging) as RL

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
    getNetwork rpd = do
      nwTarget <- Ref.new $ Network.empty "f"
      _ <- RL.runRpdLogging (flip Ref.write $ nwTarget) rpd
      Ref.read nwTarget
