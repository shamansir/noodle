module Noodle.Test.Spec.Flow.Inlets
    ( spec
    ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Data.Array (catMaybes) as Array

import Effect.Aff (delay)

import FRP.Event.Time (interval)

import FSM (run_, pushAll) as Actions

import Noodle.API.Action.Sequence ((</>))
import Noodle.API.Action.Sequence (init) as Actions
import Noodle.API.Action.Sequence as R
import Noodle.Network (empty) as Network
import Noodle.Path (toPatch, toNode, toInlet)
import Noodle.Util (flow) as R

import Effect.Class (liftEffect)

import Test.Spec (Spec, it, pending, pending', itOnly)
import Test.Spec.Assertions (shouldEqual, shouldContain, shouldNotContain)

import Noodle.Test.Util.Spy (trace, with, get, contramap, reset) as Spy
import Noodle.Test.Util.Trace (TraceItem(..), collectData)
import Noodle.Test.Spec.Flow.Base (Delivery(..), Pipe(..), Node(..), Actions, mySequencer)


{- ======================================= -}
{- =============== INLETS ================ -}
{- ======================================= -}

spec :: Spec Unit
spec = do

  -- INLETS --

  it "we receive no data from the inlet when it has no flow" $ do
    actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData

    { push } <- liftEffect $ Actions.run_
        mySequencer
        (pure $ Network.empty "foo")
        (Spy.with actionTraceSpy)

    liftEffect
           $  Actions.pushAll push
           $  Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node" Empty
          </> R.addInlet (toNode "patch" "node") "inlet" Pass

    delay $ Milliseconds 100.0

    collectedData <- liftEffect $ Spy.get actionTraceSpy
    collectedData `shouldEqual` []

  pending "we receive the default value of the inlet just when it was set"

  it "we receive the data sent directly to the inlet" $ do
    let firstInlet = toInlet "patch" "node" "inlet"

    actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData

    { push } <- liftEffect $ Actions.run_
        mySequencer
        (pure $ Network.empty "foo")
        (Spy.with actionTraceSpy)

    liftEffect
           $  Actions.pushAll push
           $  Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node" Empty
          </> R.addInlet (toNode "patch" "node") "inlet" Pass
          </> R.sendToInlet firstInlet Parcel
          </> R.sendToInlet firstInlet Pills
          </> R.sendToInlet firstInlet (Curse 5)

    delay $ Milliseconds 100.0

    collectedData <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy
    collectedData `shouldEqual`
        [ InletData firstInlet Parcel
        , InletData firstInlet Pills
        , InletData firstInlet (Curse 5)
        ]

  it "we receive the values from the data stream attached to the inlet" $ do
    actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData

    { push } <- liftEffect $ Actions.run_
        mySequencer
        (pure $ Network.empty "foo")
        (Spy.with actionTraceSpy)

    liftEffect
           $  Actions.pushAll push
           $  Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node" Empty
          </> R.addInlet (toNode "patch" "node") "inlet" Pass
          </> R.streamToInlet
                (toInlet "patch" "node" "inlet")
                (R.flow $ const Pills <$> interval 30)

    delay $ Milliseconds 100.0

    collectedData <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy
    collectedData `shouldContain`
        (InletData (toInlet "patch" "node" "inlet") Pills)

  pending "it is possible to manually cancel the streaming-to-inlet procedure"

  it "attaching several simultaneous streams to the inlet allows them to overlap" $ do
    let firstInlet = toInlet "patch" "node" "inlet"

    actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData

    { push } <- liftEffect $ Actions.run_
        mySequencer
        (pure $ Network.empty "foo")
        (Spy.with actionTraceSpy)

    liftEffect
           $  Actions.pushAll push
           $  Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node" Empty
          </> R.addInlet (toNode "patch" "node") "inlet" Pass
          </> R.streamToInlet
                (toInlet "patch" "node" "inlet")
                (R.flow $ const Pills <$> interval 20)
          </> R.streamToInlet
                (toInlet "patch" "node" "inlet")
                (R.flow $ const Banana <$> interval 29)

    delay $ Milliseconds 100.0

    collectedData <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy
    collectedData `shouldContain`
      (InletData (toInlet "patch" "node" "inlet") Pills)
    collectedData `shouldContain`
      (InletData (toInlet "patch" "node" "inlet") Banana)

  -- TODO: could be replaced, since for now user has no control over stopping the stream
  pending' "when there are no incoming streams anymore, values are not sent to the inlet" $ do
    actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData

    { push } <- liftEffect $ Actions.run_
        mySequencer
        (pure $ Network.empty "foo")
        (Spy.with actionTraceSpy)

    liftEffect
           $  Actions.pushAll push
           $  Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node" Empty
          </> R.addInlet (toNode "patch" "node") "inlet" Pass
          </> R.streamToInlet
                (toInlet "patch" "node" "inlet")
                (R.flow $ const Pills <$> interval 20)

    delay $ Milliseconds 100.0

    collectedData <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy
    collectedData `shouldContain`
      (InletData (toInlet "patch" "node" "inlet") Pills)

  it "two different streams may work for different inlets" $ do
    actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData

    { push } <- liftEffect $ Actions.run_
        mySequencer
        (pure $ Network.empty "foo")
        (Spy.with actionTraceSpy)

    liftEffect
           $  Actions.pushAll push
           $  Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node" Empty
          </> R.addInlet (toNode "patch" "node") "for-pills" Pass
          </> R.addInlet (toNode "patch" "node") "for-bananas" Pass
          </> R.streamToInlet
                (toInlet "patch" "node" "for-pills")
                (R.flow $ const Pills <$> interval 30)
          </> R.streamToInlet
                (toInlet "patch" "node" "for-bananas")
                (R.flow $ const Banana <$> interval 25)

    delay $ Milliseconds 100.0

    collectedData <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy
    collectedData `shouldContain`
      (InletData (toInlet "patch" "node" "for-pills") Pills)
    collectedData `shouldContain`
      (InletData (toInlet "patch" "node" "for-bananas") Banana)
    pure unit

  it "same stream may produce values for several inlets" $ do
    let stream = R.flow $ const Banana <$> interval 25

    actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData

    { push } <- liftEffect $ Actions.run_
        mySequencer
        (pure $ Network.empty "foo")
        (Spy.with actionTraceSpy)

    liftEffect
           $  Actions.pushAll push
           $  Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node" Empty
          </> R.addInlet (toNode "patch" "node") "inlet1" Pass
          </> R.addInlet (toNode "patch" "node") "inlet2" Pass
          </> R.streamToInlet (toInlet "patch" "node" "inlet1") stream
          </> R.streamToInlet (toInlet "patch" "node" "inlet2") stream

    delay $ Milliseconds 100.0

    collectedData <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy
    collectedData `shouldContain`
      (InletData (toInlet "patch" "node" "inlet1") Banana)
    collectedData `shouldContain`
      (InletData (toInlet "patch" "node" "inlet2") Banana)
    pure unit


  it "when inlet was removed, it stops receiving data" $ do
    let stream = R.flow $ const Banana <$> interval 25

    actionTraceSpy <- liftEffect $ Spy.trace <#> Spy.contramap collectData

    { push } <- liftEffect $ Actions.run_
        mySequencer
        (pure $ Network.empty "foo")
        (Spy.with actionTraceSpy)

    liftEffect
           $  Actions.pushAll push
           $  Actions.init
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node" Empty
          </> R.addInlet (toNode "patch" "node") "inlet" Pass
          </> R.streamToInlet (toInlet "patch" "node" "inlet") stream

    delay $ Milliseconds 100.0

    collectedData <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy
    collectedData `shouldContain`
      (InletData (toInlet "patch" "node" "inlet") Banana)

    _ <- liftEffect $ Spy.reset actionTraceSpy

    liftEffect $ push $ R.removeInlet (toInlet "patch" "node" "inlet")

    delay $ Milliseconds 100.0

    collectedData' <- liftEffect $ Array.catMaybes <$> Spy.get actionTraceSpy
    collectedData' `shouldNotContain`
      (InletData (toInlet "patch" "node" "inlet") Banana)

    collectedData' `shouldEqual` []

  pending "sending data to the inlet triggers the processing function of the node"

  pending "receiving data from the stream triggers the processing function of the node"

  pending "on connection, latest value of the outlet is sent to the inlet's flow"

  pending "if default value of the inlet is specified, then it is sent to its flow when node was created"

  pending "if default value of the inlet is specified, the inlet switches back to this value when everyone is diconnected"

  pending "when we connect several streams to the inlet (and it allows such connections), the data is received in parallel from all of them"

  pending "when we connect new stream to the inlet and it is already connected (and in 'one-connection-only' mode), the data from the newly-connected stream replaces the data from the previous one"

