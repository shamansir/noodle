module RpdTest.Flow.Nodes
    ( spec
    ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Map as Map
import Data.Lens ((^.))
import Data.Lens.At (at)

import Rpd.API ((</>))
import Rpd.API as R
import Rpd.Process as R
import Rpd.Path

import Test.Spec (Spec, it, pending)
import Test.Spec.Assertions (shouldContain)

import RpdTest.Util (withRpd)
import RpdTest.CollectData (TraceItem(..))
import RpdTest.CollectData as CollectData
import RpdTest.Flow.Base
    ( MyRpd, Delivery(..)
    , sumCursesToApplesNode
    , sumCursesToApplesNode'
    )


{- ======================================= -}
{- ================ NODES ================ -}
{- ======================================= -}


spec :: Spec Unit
spec = do
  pending "adding an inlet inludes its flow into processing"

  it "returning some value from processing function actually sends this value to the outlet (array way)" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode' (patchId 0)
                (sumCursesToApplesNode (R.FoldedByIndex process))
      process (R.InletsData [ Just (Curse a), Just (Curse b) ]) =
        R.OutletsData [ Apple (a + b) ]
      process _ = R.OutletsData [ Apple 9 ]

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            _ <- nw  #  R.sendToInlet (inletPath 0 0 0) (Curse 4)
                    </> R.sendToInlet (inletPath 0 0 1) (Curse 3)
            pure [ ]
        collectedData `shouldContain`
          (InletData (inletPath 0 0 0) $ Curse 4)
        collectedData `shouldContain`
          (InletData (inletPath 0 0 1) $ Curse 3)
        collectedData `shouldContain`
          (OutletData (outletPath 0 0 0) $ Apple 7)
        pure unit

    pure unit

  it "returning some value from processing function actually sends this value to the outlet (labels way)" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode' (patchId 0) (sumCursesToApplesNode (R.FoldedByLabel process))
      processHelper (Curse a) (Curse b) =
        Map.insert "apples" (Apple (a + b)) Map.empty
      processHelper _ _ =
        Map.empty
      process (R.InletsMapData m) =
        R.OutletsMapData
          $ fromMaybe Map.empty
          $ processHelper <$> (m^.at "curse1") <*> (m^.at "curse2")

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            _ <- nw  #  R.sendToInlet (inletPath 0 0 0) (Curse 4)
                    </> R.sendToInlet (inletPath 0 0 1) (Curse 3)
            pure [ ]
        collectedData `shouldContain`
          (InletData (inletPath 0 0 0) $ Curse 4)
        collectedData `shouldContain`
          (InletData (inletPath 0 0 1) $ Curse 3)
        collectedData `shouldContain`
          (OutletData (outletPath 0 0 0) $ Apple 7)
        pure unit

    pure unit


  it "returning multiple values from processing function actually sends these values to the outlets (array way)" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode' (patchId 0)
                (sumCursesToApplesNode' (R.FoldedByIndex process))
      process (R.InletsData [ Just (Curse a), Just (Curse b) ]) =
        R.OutletsData [ Apple (a + b), Apple (a - b) ]
      process _ = R.OutletsData [ ]

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            _ <- nw  #  R.sendToInlet (inletPath 0 0 0) (Curse 4)
                    </> R.sendToInlet (inletPath 0 0 1) (Curse 3)
            pure [ ]
        collectedData `shouldContain`
          (InletData (inletPath 0 0 0) $ Curse 4)
        collectedData `shouldContain`
          (InletData (inletPath 0 0 1) $ Curse 3)
        collectedData `shouldContain`
          (OutletData (outletPath 0 0 0) $ Apple 7)
        collectedData `shouldContain`
          (OutletData (outletPath 0 0 1) $ Apple 1)
        pure unit

    pure unit


  it "returning multiple values from processing function actually sends these values to the outlets (label way)" $ do
    let
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode' (patchId 0)
                (sumCursesToApplesNode' (R.FoldedByLabel process))
      processHelper (Curse a) (Curse b) =
        Map.empty
          # Map.insert "apples1" (Apple (a + b))
          # Map.insert "apples2" (Apple (a - b))
      processHelper _ _ =
        Map.empty
      process (R.InletsMapData m) =
        R.OutletsMapData
          $ fromMaybe Map.empty
          $ processHelper <$> (m^.at "curse1") <*> (m^.at "curse2")

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            _ <- nw  #  R.sendToInlet (inletPath 0 0 0) (Curse 4)
                    </> R.sendToInlet (inletPath 0 0 1) (Curse 3)
            pure [ ]
        collectedData `shouldContain`
          (InletData (inletPath 0 0 0) $ Curse 4)
        collectedData `shouldContain`
          (InletData (inletPath 0 0 1) $ Curse 3)
        collectedData `shouldContain`
          (OutletData (outletPath 0 0 0) $ Apple 7)
        collectedData `shouldContain`
          (OutletData (outletPath 0 0 1) $ Apple 1)
        pure unit

    pure unit
