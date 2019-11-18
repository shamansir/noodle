module RpdTest.Flow.Nodes
    ( spec
    ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))

import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import FRP.Event as Event
import FRP.Event.Time (interval)

import Rpd.Util (flow) as R
import Rpd.API.Action.Sequence ((</>))
import Rpd.API.Action.Sequence (init) as Actions
import Rpd.API.Action.Sequence as R
import Rpd.Process (ProcessF(..), makeProcessST) as R
import Rpd.Path
import Rpd.Network (empty) as Network
import Rpd.Toolkit
    ( (>~), (~<)
    , withInlets, withOutlets
    , noInlets, noOutlets
    , NodeDef(..)
    )

import Test.Spec (Spec, it, pending, describe)
import Test.Spec.Assertions (shouldContain, shouldNotContain, shouldEqual, shouldNotEqual)

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

  describe "processing" $ do

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


    it "when node has no outlets, but has some inlets, processing is still performed" $ do
      valueRef <- liftEffect $ Ref.new Damaged

      let
        curseInlet = toInlet "patch" "node" "curse"

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
                  ~< "curse" /\ Pass
              , outlets :
                  noOutlets
              , process : R.Process processF
              }

        processF receive = do
            -- FIXME: rewrite using `<$>`
            let
                curse = receive "curse" # fromMaybe Damaged
            _ <- valueRef # Ref.write curse
            pure $ const Nothing

      _ /\ collectedData <- CollectData.channelsAfter
        (Milliseconds 100.0)
        myToolkit
        (Network.empty "network")
        $ structure
            </> R.sendToInlet curseInlet (Curse 4)

      storedValue <- liftEffect $ Ref.read valueRef

      collectedData `shouldContain`
        (InletData curseInlet $ Curse 4)

      storedValue `shouldEqual` (Curse 4)

      pure unit


    it "when node has both no outlets and inlets, processing is not performed" $ do
      wasCalledRef <- liftEffect $ Ref.new false

      let
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
                  noInlets
              , outlets :
                  noOutlets
              , process : R.Process processF
              }

        processF receive = do
            -- FIXME: rewrite using `<$>`
            _ <- wasCalledRef # Ref.write true
            pure $ const Nothing

      _ /\ collectedData <- CollectData.channelsAfter
        (Milliseconds 100.0)
        myToolkit
        (Network.empty "network")
        structure

      wasCalled <- liftEffect $ Ref.read wasCalledRef

      collectedData `shouldEqual` []

      wasCalled `shouldEqual` false

      pure unit


    it "processing with state works" $ do
        let

          curseInlet = toInlet "patch" "node" "curse"
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
                    ~< "curse" /\ Pass
                , outlets :
                    withOutlets
                    >~ "apples" /\ Pass
                , process : R.ProcessST $ R.makeProcessST 10 processF
                }

          processF (prevState /\ receive) = do
              -- FIXME: rewrite using `<$>`
              let
                  curse = receive "curse" # fromMaybe Damaged
              pure $ case curse of
                  (Curse c) ->
                      let send "apples" = Just $ Apple (prevState + c)
                          send _ = Nothing
                      in (prevState + c) /\ send
                  _ -> prevState /\ const Nothing

        _ /\ collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          myToolkit
          (Network.empty "network")
          $ structure
              </> R.sendToInlet curseInlet (Curse 4)
              </> R.sendToInlet curseInlet (Curse 3)

        collectedData `shouldContain`
          (InletData curseInlet $ Curse 4)
        collectedData `shouldContain`
          (InletData curseInlet $ Curse 3)
        collectedData `shouldContain`
          (OutletData applesOutlet $ Apple 14)
        collectedData `shouldContain`
          (OutletData applesOutlet $ Apple 17)

        pure unit

  describe "removing" $ do

      it "when node was removed, its inlets stop receiving data" $ do
        let
          curseInlet = toInlet "patch" "node" "curse"

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
                    ~< "curse" /\ Pass
                , outlets :
                    noOutlets
                , process : R.Withhold
                }

        let
          nwWithFlow =
            structure </>
              R.streamToInlet curseInlet (R.flow $ const (Curse 4) <$> interval 30)

        nw' /\ collectedData <- CollectData.channelsAfter
          (Milliseconds 100.0)
          myToolkit
          (Network.empty "network")
          nwWithFlow

        collectedData `shouldContain`
          (InletData curseInlet $ Curse 4)

        _ /\ collectedData' <- CollectData.channelsAfter
          (Milliseconds 100.0)
          myToolkit
          (Network.empty "network")
          $ nwWithFlow </> R.removeInlet curseInlet

        collectedData' `shouldNotContain`
          (InletData curseInlet $ Curse 4)

        collectedData' `shouldEqual` []

        pure unit
