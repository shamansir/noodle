module RpdTest.Flow.Nodes
    ( spec
    ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Map as Map
import Data.Lens ((^.))
import Data.Lens.At (at)
import Data.Tuple.Nested ((/\))

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

  it "returning some value from processing function actually sends this value to the outlet" $ do
    let
      curse1Inlet = toInlet "patch" "node" "curse1"
      curse2Inlet = toInlet "patch" "node" "curse2"
      applesOutlet = toOutlet "patch" "node" "apples"
      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addDefNode (toPatch "patch") "node"
                (sumCursesToApplesNode $ R.Process processF)
      processF receive = do
          let
              curse1 = receive "curse1" # fromMaybe Damaged
              curse2 = receive "curse2" # fromMaybe Damaged
          sumOrDamage <-
              case curse1 /\ curse2 of
                  (Curse c1 /\ Curse c2) ->
                      pure $ Apple (c1 + c2)
                  _ -> pure Damaged
          let send "apples" = Just sumOrDamage
              send _ = Nothing
          pure send

      -- process (R.InletsData [ Just (Curse a), Just (Curse b) ]) =
      --   R.OutletsData [ Apple (a + b) ]
      -- process _ = R.OutletsData [ Apple 9 ]

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            _ <- nw  #  R.sendToInlet curse1Inlet (Curse 4)
                    </> R.sendToInlet curse2Inlet (Curse 3)
            pure [ ]
        collectedData `shouldContain`
          (InletData curse1Inlet $ Curse 4)
        collectedData `shouldContain`
          (InletData curse2Inlet $ Curse 3)
        collectedData `shouldContain`
          (OutletData applesOutlet $ Apple 7)
        pure unit

    pure unit


  it "returning multiple values from processing function actually sends these values to the outlets (array way)" $ do
    let
      curse1Inlet = toInlet "patch" "node" "curse1"
      curse2Inlet = toInlet "patch" "node" "curse2"
      apples1Outlet = toOutlet "patch" "node" "apples1"
      apples2Outlet = toOutlet "patch" "node" "apples2"

      rpd :: MyRpd
      rpd =
        R.init "network"
          </> R.addPatch "patch"
          </> R.addDefNode (toPatch "patch") "node"
                (sumCursesToApplesNode' $ R.Process processF)
      processF receive = do
          let
              curse1 = receive "curse1" # fromMaybe Damaged
              curse2 = receive "curse2" # fromMaybe Damaged
          case curse1 /\ curse2 of
              (Curse c1 /\ Curse c2) ->
                  let send "apples1" = Just $ Apple (c1 + c2)
                      send "apples2" = Just $ Apple (c1 - c2)
                      send _ = Nothing
                  in pure send
              _ -> pure $ const Nothing

    rpd # withRpd \nw -> do
        collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          nw
          $ do
            _ <- nw  #  R.sendToInlet curse1Inlet (Curse 4)
                    </> R.sendToInlet curse2Inlet (Curse 3)
            pure [ ]
        collectedData `shouldContain`
          (InletData curse1Inlet $ Curse 4)
        collectedData `shouldContain`
          (InletData curse2Inlet $ Curse 3)
        collectedData `shouldContain`
          (OutletData apples1Outlet $ Apple 7)
        collectedData `shouldContain`
          (OutletData apples2Outlet $ Apple 1)
        pure unit

    pure unit
