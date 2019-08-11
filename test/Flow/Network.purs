module RpdTest.Flow.Network
    ( spec
    ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))

import Test.Spec (Spec, it, pending)
import Test.Spec.Assertions (shouldEqual)

import Rpd.Network as Network
import Rpd.API.Action.Sequence as Actions

import RpdTest.Flow.Base (myToolkit)
import RpdTest.Helper (channelsAfter)


{- ======================================= -}
{- =============== NETWORK =============== -}
{- ======================================= -}


spec :: Spec Unit
spec = do
  it "we receive no data from the network when it's empty" $ do
    collectedData <- channelsAfter (Milliseconds 100.0) myToolkit (Network.empty "no-data") Actions.init
    collectedData `shouldEqual` []

  pending "all the cancelers are called after running the system"

