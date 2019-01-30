module RpdTest.Flow.Network
    ( spec
    ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))

import Rpd.API (init) as R

import Test.Spec (Spec, it, pending)
import Test.Spec.Assertions (shouldEqual)

import RpdTest.Util (withRpd)
import RpdTest.CollectData as CollectData
import RpdTest.Flow.Base (MyRpd)


{- ======================================= -}
{- =============== NETWORK =============== -}
{- ======================================= -}


spec :: Spec Unit
spec = do
  it "we receive no data from the network when it's empty" $ do
    (R.init "no-data" :: MyRpd)
      # withRpd \nw -> do
          collectedData <- nw #
            CollectData.channels (Milliseconds 100.0)
          collectedData `shouldEqual` []
          pure unit

    pure unit

  pending "all the cancelers are called after running the system"

