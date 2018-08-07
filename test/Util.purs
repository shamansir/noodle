module Test.Util
    ( runWith
    , TestAffE, TestAff
    ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)

import FRP (FRP)

import Rpd (Network) as Rpd

type TestAffE e = (ref :: REF, frp :: FRP, console :: CONSOLE | e)
type TestAff e = Aff (TestAffE e) Unit

runWith :: forall e d. Rpd.Network d e -> (Rpd.Network d e -> TestAff e) -> TestAff e
runWith initialNetwork f = f initialNetwork

-- runWith :: forall e d. Rpd.Network d -> (Rpd.Network d -> TestAff e) -> TestAff e
-- runWith initialNetwork f = do
--   newNetwork <- liftEff $ do
--     networkRef <- newRef Rpd.empty
--     Rpd.run (writeRef networkRef) initialNetwork
--     readRef networkRef
--   f newNetwork
