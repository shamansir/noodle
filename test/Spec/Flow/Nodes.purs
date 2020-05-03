module Noodle.Test.Spec.Flow.Nodes
    ( spec
    ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Data.Array (catMaybes) as Array

import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Aff (delay)

import FRP.Event as Event
import FRP.Event.Time (interval)

import FSM (run_, pushAll) as Actions

import Noodle.Util (flow) as R
import Noodle.API.Action.Sequence ((</>))
import Noodle.API.Action.Sequence (init) as Actions
import Noodle.API.Action.Sequence as R
import Noodle.Process (ProcessF(..), makeProcessST) as R
import Noodle.Path
import Noodle.Network (empty) as Network
import Noodle.Toolkit
    ( (>~), (~<)
    , withInlets, withOutlets
    , noInlets, noOutlets
    , NodeDef(..)
    )

import Test.Spec (Spec, it, pending, pending', describe, itOnly)
import Test.Spec.Assertions (shouldContain, shouldNotContain, shouldEqual, shouldNotEqual)

import Noodle.Test.Util.Spy (trace, with, get, contramap, reset) as Spy
import Noodle.Test.Util.Trace (TraceItem(..), collectData)
import Noodle.Test.Spec.Flow.Base
    ( Actions
    , Delivery(..), Pipe(..), Node(..)
    , mySequencer
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

      actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData

      { push } <- liftEffect $ Actions.run_
        mySequencer
        (pure $ Network.empty "foo")
        (Spy.with actionTraceSpy)

      liftEffect
             $  Actions.pushAll push
             $  Actions.init
            </> R.addPatch "patch"
            </> R.addNodeByDef
                    (toPatch "patch")
                    "node"
                    Custom
                    nodeDef
            </> R.sendToInlet curse1Inlet (Curse 4)
            </> R.sendToInlet curse2Inlet (Curse 3)

      collectedData <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy
      collectedData `shouldContain`
        (InletData curse1Inlet $ Curse 4)
      collectedData `shouldContain`
        (InletData curse2Inlet $ Curse 3)
      collectedData `shouldContain`
        (OutletData applesOutlet $ Apple 7)

    it "returning multiple values from processing function actually sends these values to the outlets" $ do
      let

        curse1Inlet = toInlet "patch" "node" "curse1"
        curse2Inlet = toInlet "patch" "node" "curse2"
        apples1Outlet = toOutlet "patch" "node" "apples1"
        apples2Outlet = toOutlet "patch" "node" "apples2"

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

      actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData

      { push } <- liftEffect $ Actions.run_
        mySequencer
        (pure $ Network.empty "foo")
        (Spy.with actionTraceSpy)

      liftEffect
             $  Actions.pushAll push
             $  Actions.init
            </> R.addPatch "patch"
            </> R.addNodeByDef
                    (toPatch "patch")
                    "node"
                    Custom
                    nodeDef
            </> R.sendToInlet curse1Inlet (Curse 4)
            </> R.sendToInlet curse2Inlet (Curse 3)

      collectedData <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy

      collectedData `shouldContain`
        (InletData curse1Inlet $ Curse 4)
      collectedData `shouldContain`
        (InletData curse2Inlet $ Curse 3)
      collectedData `shouldContain`
        (OutletData apples1Outlet $ Apple 7)
      collectedData `shouldContain`
        (OutletData apples2Outlet $ Apple 1)


    it "when node has no outlets, but has some inlets, processing is still performed" $ do
      valueRef <- liftEffect $ Ref.new Damaged

      let
        curseInlet = toInlet "patch" "node" "curse"

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

      actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData

      { push } <- liftEffect $ Actions.run_
        mySequencer
        (pure $ Network.empty "foo")
        (Spy.with actionTraceSpy)

      liftEffect
             $  Actions.pushAll push
             $  Actions.init
            </> R.addPatch "patch"
            </> R.addNodeByDef
                    (toPatch "patch")
                    "node"
                    Custom
                    nodeDef
            </> R.sendToInlet curseInlet (Curse 4)

      collectedData <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy

      storedValue <- liftEffect $ Ref.read valueRef

      collectedData `shouldContain`
        (InletData curseInlet $ Curse 4)

      storedValue `shouldEqual` (Curse 4)


    it "when node has both no outlets and inlets, processing is not performed" $ do
      wasCalledRef <- liftEffect $ Ref.new false -- FIXME: rewrite using Spy.

      let
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

      actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData

      { push } <- liftEffect $ Actions.run_
        mySequencer
        (pure $ Network.empty "foo")
        (Spy.with actionTraceSpy)

      liftEffect
             $  Actions.pushAll push
             $  Actions.init
            </> R.addPatch "patch"
            </> R.addNodeByDef
                  (toPatch "patch")
                  "node"
                  Custom
                  nodeDef

      delay $ Milliseconds 100.0

      collectedData <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy

      wasCalled <- liftEffect $ Ref.read wasCalledRef

      collectedData `shouldEqual` []

      wasCalled `shouldEqual` false


    it "processing with state works" $ do
      let

        curseInlet = toInlet "patch" "node" "curse"
        applesOutlet = toOutlet "patch" "node" "apples"

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

      actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData

      { push } <- liftEffect $ Actions.run_
        mySequencer
        (pure $ Network.empty "foo")
        (Spy.with actionTraceSpy)

      liftEffect
             $  Actions.pushAll push
             $  Actions.init
            </> R.addPatch "patch"
            </> R.addNodeByDef
                    (toPatch "patch")
                    "node"
                    Custom
                    nodeDef
            </> R.sendToInlet curseInlet (Curse 4)
            </> R.sendToInlet curseInlet (Curse 3)

      collectedData <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy

      collectedData `shouldContain`
        (InletData curseInlet $ Curse 4)
      collectedData `shouldContain`
        (InletData curseInlet $ Curse 3)
      collectedData `shouldContain`
        (OutletData applesOutlet $ Apple 14)
      collectedData `shouldContain`
        (OutletData applesOutlet $ Apple 17)

  describe "removing" $ do

    pending "when node was removed, its processing function is not called anymore"

    it "when node was removed, its inlets stop receiving data" $ do
      let
        curseInlet = toInlet "patch" "node" "curse"

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

      actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData

      { push } <- liftEffect $ Actions.run_
        mySequencer
        (pure $ Network.empty "foo")
        (Spy.with actionTraceSpy)

      liftEffect
             $  Actions.pushAll push
             $  Actions.init
            </> R.addPatch "patch"
            </> R.addNodeByDef
                    (toPatch "patch")
                    "node"
                    Custom
                    nodeDef
            </> R.streamToInlet curseInlet
                    (R.flow $ const (Curse 4) <$> interval 30)

      delay $ Milliseconds 100.0

      collectedData <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy

      collectedData `shouldContain`
        (InletData curseInlet $ Curse 4)

      _ <- liftEffect $ Spy.reset actionTraceSpy

      liftEffect $ push $ R.removeNode (toNode "patch" "node")

      delay $ Milliseconds 100.0

      collectedData' <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy

      collectedData' `shouldNotContain`
        (InletData curseInlet $ Curse 4)

      collectedData' `shouldEqual` []


    it "when node was removed, its outgoing connections are deactivated and do not receive any data" $ do
      let
        curseOutlet = toOutlet "patch" "subject" "curse"
        curseInlet = toInlet "patch" "other" "curse"

        subjectNodeDef :: NodeDef Delivery Pipe
        subjectNodeDef =
            NodeDef
              { inlets :
                  noInlets
              , outlets :
                  withOutlets
                  >~ "curse" /\ Pass
              , process : R.Withhold
              }

        otherNodeDef :: NodeDef Delivery Pipe
        otherNodeDef =
            NodeDef
              { inlets :
                  withInlets
                  ~< "curse" /\ Pass
              , outlets :
                  noOutlets
              , process : R.Withhold
              }

      actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData

      { push } <- liftEffect $ Actions.run_
        mySequencer
        (pure $ Network.empty "foo")
        (Spy.with actionTraceSpy)

      liftEffect
             $  Actions.pushAll push
             $  Actions.init
            </> R.addPatch "patch"
            </> R.addNodeByDef
                  (toPatch "patch")
                  "subject"
                  Custom
                  subjectNodeDef
            </> R.addNodeByDef
                  (toPatch "patch")
                  "other"
                  Custom
                  otherNodeDef
            </> R.connect
                  curseOutlet
                  curseInlet
            </> R.streamToOutlet curseOutlet
                  (R.flow $ const (Curse 4) <$> interval 30)

      delay $ Milliseconds 100.0

      collectedData <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy

      collectedData `shouldContain`
        (InletData curseInlet $ Curse 4)

      _ <- liftEffect $ Spy.reset actionTraceSpy

      liftEffect $ push $ R.removeNode (toNode "patch" "subject")

      delay $ Milliseconds 100.0

      collectedData' <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy

      collectedData' `shouldNotContain`
        (InletData curseInlet $ Curse 4)

      collectedData' `shouldEqual` []

    -- FIXME: the values which are streamed this way are staying for the moment,
    --        for some reason we have to cancel the user stream itself,
    --        not only the pushing subscription, to get rid of this behavior
    it "when node was removed, its ingoing connections are deactivated and do not receive any data" $ do
      let
        curseOutlet = toOutlet "patch" "other" "curse"
        curseInlet = toInlet "patch" "subject" "curse"

        subjectNodeDef :: NodeDef Delivery Pipe
        subjectNodeDef =
            NodeDef
              { inlets :
                  withInlets
                  ~< "curse" /\ Pass
              , outlets :
                  noOutlets
              , process : R.Withhold
              }

        otherNodeDef :: NodeDef Delivery Pipe
        otherNodeDef =
            NodeDef
              { inlets :
                  noInlets
              , outlets :
                  withOutlets
                  >~ "curse" /\ Pass
              , process : R.Withhold
              }

      actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData

      { push } <- liftEffect $ Actions.run_
        mySequencer
        (pure $ Network.empty "foo")
        (Spy.with actionTraceSpy)

      liftEffect
             $  Actions.pushAll push
             $  Actions.init
            </> R.addPatch "patch"
            </> R.addNodeByDef
                    (toPatch "patch")
                    "subject"
                    Custom
                    subjectNodeDef
            </> R.addNodeByDef
                    (toPatch "patch")
                    "other"
                    Custom
                    otherNodeDef
            </> R.connect
                  curseOutlet
                  curseInlet
            </> R.streamToOutlet curseOutlet (R.flow $ const (Curse 4) <$> interval 30)

      delay $ Milliseconds 100.0

      collectedData <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy

      collectedData `shouldContain`
        (InletData curseInlet $ Curse 4)

      _ <- liftEffect $ Spy.reset actionTraceSpy

      liftEffect $ push $ R.removeNode (toNode "patch" "subject")

      delay $ Milliseconds 100.0

      collectedData' <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy

      collectedData' `shouldNotContain`
        (InletData curseInlet $ Curse 4)
