module Test.Util
    ( runWith
    ) where

import Prelude

import Effect.Aff (Aff)

import Rpd (Network) as Rpd

runWith :: forall d. Rpd.Network d -> (Rpd.Network d -> Aff Unit) -> Aff Unit
runWith initialNetwork f = f initialNetwork

-- runWith :: forall e d. Rpd.Network d -> (Rpd.Network d -> TestAff e) -> TestAff e
-- runWith initialNetwork f = do
--   newNetwork <- liftEff $ do
--     networkRef <- newRef Rpd.empty
--     Rpd.run (writeRef networkRef) initialNetwork
--     readRef networkRef
--   f newNetwork
