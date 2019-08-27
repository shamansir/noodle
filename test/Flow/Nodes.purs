module RpdTest.Flow.Nodes
    ( spec
    ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))

import Rpd.API.Action.Sequence ((</>))
import Rpd.API.Action.Sequence (init) as Actions
import Rpd.API.Action.Sequence as R
import Rpd.Process (ProcessF(..)) as R
import Rpd.Path
import Rpd.Network (empty) as Network
import Rpd.Toolkit ((>~), (~<), withInlets, withOutlets, NodeDef(..))

import Test.Spec (Spec, it, pending)
import Test.Spec.Assertions (shouldContain)

import RpdTest.Helper (TraceItem(..))
import RpdTest.Helper (channelsAfter) as CollectData
import RpdTest.Flow.Base
    ( Actions
    , Delivery(..), Pipe(..), Node(..)
    , myToolkit
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

      structure :: Actions
      structure =
        Actions.init
          </> R.addPatch "patch"
          </> R.addNodeByDef
                  (toPatch "patch")
                  "node"
                  Custom
                  nodeDef

      nodeDef :: NodeDef Delivery Pipe
      nodeDef =
          NodeDef
            { inlets :
                withInlets
                ~< "curse1" /\ Pass
                ~< "curse2" /\ Pass
            , outlets :
                withOutlets
                >~ "apples" /\ Pass
            , process : R.Process processF
            }

      processF receive = do
          -- FIXME: rewrite using `<$>`
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

    _ /\ collectedData <-
      CollectData.channelsAfter
          (Milliseconds 100.0)
          myToolkit
          (Network.empty "network")
          $ structure
              </> R.sendToInlet curse1Inlet (Curse 4)
              </> R.sendToInlet curse2Inlet (Curse 3)

    collectedData `shouldContain`
      (InletData curse1Inlet $ Curse 4)
    collectedData `shouldContain`
      (InletData curse2Inlet $ Curse 3)
    collectedData `shouldContain`
      (OutletData applesOutlet $ Apple 7)

    pure unit


  it "returning multiple values from processing function actually sends these values to the outlets" $ do
    let

      curse1Inlet = toInlet "patch" "node" "curse1"
      curse2Inlet = toInlet "patch" "node" "curse2"
      apples1Outlet = toOutlet "patch" "node" "apples1"
      apples2Outlet = toOutlet "patch" "node" "apples2"

      structure :: Actions
      structure =
        Actions.init
          </> R.addPatch "patch"
          </> R.addNodeByDef
                  (toPatch "patch")
                  "node"
                  Custom
                  nodeDef

      nodeDef :: NodeDef Delivery Pipe
      nodeDef =
          NodeDef
            { inlets :
                withInlets
                ~< "curse1" /\ Pass
                ~< "curse2" /\ Pass
            , outlets :
                withOutlets
                >~ "apples1" /\ Pass
                >~ "apples2" /\ Pass
            , process : R.Process processF
            }

      processF receive = do
          -- FIXME: rewrite using `<$>`
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

    _ /\ collectedData <- CollectData.channelsAfter
      (Milliseconds 100.0)
      myToolkit
      (Network.empty "network")
      $ structure
          </> R.sendToInlet curse1Inlet (Curse 4)
          </> R.sendToInlet curse2Inlet (Curse 3)

    collectedData `shouldContain`
      (InletData curse1Inlet $ Curse 4)
    collectedData `shouldContain`
      (InletData curse2Inlet $ Curse 3)
    collectedData `shouldContain`
      (OutletData apples1Outlet $ Apple 7)
    collectedData `shouldContain`
      (OutletData apples2Outlet $ Apple 1)

    pure unit
