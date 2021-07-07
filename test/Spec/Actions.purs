module Noodle.Test.Spec.Actions
    ( spec ) where

import Prelude

import Data.Either (Either(..))
import Data.Lens (view, preview) as L
import Data.Maybe (Maybe(..), isJust)
import Data.Sequence as Seq
import Data.Array (length)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Control.Alt ((<|>))

import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Console (log) as Console
import Effect.Ref as Ref

import FRP.Event as E

import Test.Spec (Spec, describe, it, itOnly, pending, pending')
import Test.Spec.Assertions (shouldEqual, fail)

import Data.Covered (recover)

import FSM as FSM

import Noodle.API.Action.Sequence ((</>))
import Noodle.API.Action.Sequence (addPatch, addNode, addInlet, addOutlet) as R
import Noodle.API.Action.Sequence as Actions
import Noodle.API.Action.Sequence (make) as Sequencer

import Noodle.Network (Inlet(..), Network, Node(..), Outlet(..)) as R
import Noodle.Network (empty) as N
import Noodle.Optics (_patchByPath) as L
import Noodle.Path as P
import Noodle.Test.Util.Actions (getOrFail, getOrFail', failIfNoError, failIfNoErrors)
import Noodle.Test.Util.Spy as Spy
import Noodle.Test.Util.Assertions
import Noodle.Toolkit as T


data MyData
  = Bang

data Channel = Channel

data Node = Node


toolkit :: T.Toolkit MyData Channel Node
toolkit = T.empty "foo"


network :: R.Network MyData Channel Node
network = N.empty "foo"


spec :: Spec Unit
spec =
  describe "structure" do
    let sequencer = Sequencer.make toolkit

    it "init" do
      result /\ _ <- liftEffect
        $ FSM.fold sequencer (pure network) Actions.init
      _ <- getOrFail result
      pure unit

    -- it "prepare" do
    --   pure unit

    describe "running" do

      it "handler is called with the model on performed actions" do
          handlerSpy <- liftEffect $ Spy.wasCalled

          let

              actionsList =
                  Actions.init
                      </> R.addPatch "foo"

          { stop, push } <- liftEffect
              $ FSM.run' sequencer (pure network) (Spy.with handlerSpy)
          liftEffect $ FSM.pushAll push actionsList

          handlerCalled <- liftEffect $ Spy.get handlerSpy
          handlerCalled `shouldEqual` true

          liftEffect stop

      it "handler receives error when it happened" do
          handlerSpy <- liftEffect $ Spy.ifErrorC

          let

              actionsList =
                  Actions.init
                      </> R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists

          { stop, push } <- liftEffect
              $ FSM.run' sequencer (pure network) (Spy.with handlerSpy)
          liftEffect $ FSM.pushAll push actionsList

          handlerCalled <- liftEffect $  Spy.get handlerSpy
          handlerCalled `shouldEqual` true

          liftEffect stop

      it "pushing actions works propetly" do
          handlerSpy <- liftEffect $ Spy.wasCalled

          let

            actionsList =
                  Actions.init
                      </> R.addPatch "foo"

          { push, stop } <-
            liftEffect
              $ FSM.run' sequencer (pure network) (Spy.with handlerSpy)
          liftEffect $ FSM.pushAll push actionsList

          liftEffect $ Spy.reset handlerSpy
          liftEffect $ push $ R.addPatch "bar"
          handlerCalled <- liftEffect $ Spy.get handlerSpy
          handlerCalled `shouldEqual` true

          liftEffect stop

      it "when error happened, next models still arrive" do
          handlerSpy <- liftEffect $ Spy.wasCalled
          errHandlerSpy <- liftEffect $ Spy.ifErrorC

          let

              actionsList = Actions.init
              everyStep v
                  =  Spy.consider handlerSpy v
                  <> Spy.consider errHandlerSpy v

          { push, stop } <- liftEffect
              $ FSM.run' sequencer (pure network) everyStep
          liftEffect $ FSM.pushAll push actionsList

          liftEffect $ push
              $ R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists

          errHandlerCalled <- liftEffect $ Spy.get errHandlerSpy
          errHandlerCalled `shouldEqual` true

          liftEffect $ Spy.reset handlerSpy
          liftEffect $ push $ R.addPatch "foo"
          handlerCalled <- liftEffect $ Spy.get handlerSpy
          handlerCalled `shouldEqual` true

          liftEffect stop

      pending' "when some error happened, it is reported only once" do
          -- we collect errors from now on, so this has no sense anymore
          errHandlerSpy <- liftEffect Spy.ifErrorC

          let
              actionsList = Actions.init

          { push, stop } <- liftEffect
              $ FSM.run' sequencer (pure network) (Spy.with errHandlerSpy)
          liftEffect $ FSM.pushAll push actionsList

          liftEffect $ push
              $ R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists

          liftEffect $ Spy.reset errHandlerSpy
          --liftEffect $ Spy.reset collectErrorsSpy
          -- liftEffect $ push $ R.addNode (P.toPatch "foo") "fail" Node
          liftEffect $ push $ R.addPatch "foo"
          hadError <- liftEffect $ Spy.get errHandlerSpy
          hadError `shouldEqual` false

          liftEffect stop

      it "the system recovers from errors" do
          lastSpy <- liftEffect Spy.last

          let
              actionsList = Actions.init
              lastNetworkSpy = Spy.contramap recover lastSpy

          { push, stop } <- liftEffect
              $ FSM.run' sequencer (pure network) (Spy.with lastNetworkSpy)
          liftEffect $ FSM.pushAll push actionsList

          liftEffect $ push
              $ R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists

          liftEffect $ push $ R.addPatch "foo"
          maybeLastNetwork <- liftEffect $ Spy.get lastNetworkSpy
          shouldHaveValue maybeLastNetwork
          shouldHaveValue
              (
                maybeLastNetwork
                  >>= L.preview (L._patchByPath $ P.toPatch "foo")
              )

          liftEffect stop

      it "stopping stops sending model updates" do
          handlerSpy <- liftEffect $ Spy.wasCalled

          let

            actionsList =
                  Actions.init
                      </> R.addPatch "foo"

          { push, stop } <- liftEffect
              $ FSM.run' sequencer (pure network) (Spy.with handlerSpy)
          liftEffect $ FSM.pushAll push actionsList

          liftEffect $ Spy.reset handlerSpy
          liftEffect $ push $ R.addPatch "bar"
          handlerCalled <- liftEffect $ Spy.get handlerSpy
          handlerCalled `shouldEqual` true

          liftEffect stop
          liftEffect $ Spy.reset handlerSpy
          liftEffect $ push $ R.addPatch "buz"
          handlerCalled' <- liftEffect $ Spy.get handlerSpy
          handlerCalled' `shouldEqual` false

          pure unit

    describe "folding" do

      it "result is containing the network when actions are successful" do
        let
          actionsList =
                Actions.init
                    </> R.addPatch "foo"

        result /\ { stop } <- liftEffect
            $ FSM.fold sequencer (pure network) actionsList

        _ <- getOrFail result

        liftEffect stop

      it "result is containing the error when it happened" do
        let
          actionsList =
                Actions.init
                    </> R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists

        result /\ { stop } <- liftEffect
            $ FSM.fold sequencer (pure network) actionsList

        failIfNoError "no error" result

        liftEffect stop

      pending' "result is containing the last success, even if some error happened" do
        let
          actionsList =
                Actions.init
                    </> R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists
                    </> R.addPatch "bar"

        result /\ { stop } <- liftEffect
            $ FSM.fold sequencer (pure network) actionsList

        _ <- getOrFail result

        liftEffect stop

      it "result contains the error happened last, even if there were successes after" do
        let
          actionsList =
                Actions.init
                    </> R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists
                    </> R.addPatch "bar"

        result /\ { stop } <- liftEffect
            $ FSM.fold sequencer (pure network) actionsList

        failIfNoError "no error" result

        liftEffect stop

      pending "result contains all the errors happened"

      pending' "pushing actions works propetly" do
        handlerSpy <- liftEffect $ Spy.wasCalled

        let

          actionsList =
                Actions.init
                    </> R.addPatch "foo"

        -- nothing to listen for

        { push, stop } <- liftEffect
            $ FSM.run sequencer (pure network)
        liftEffect $ FSM.pushAll push actionsList

        liftEffect $ Spy.reset handlerSpy
        liftEffect $ push $ R.addPatch "bar"
        handlerCalled <- liftEffect $ Spy.get handlerSpy
        handlerCalled `shouldEqual` true

        liftEffect stop

    describe "tracing" do

      it "it is possible to handle actions from the given actions list" do
        handlerSpy <- liftEffect $ Spy.wasCalled

        let
          actionsList =
                Actions.init
                  </> R.addPatch "foo"

        { stop, push } <- liftEffect
            $ FSM.run' sequencer (pure network) (Spy.with handlerSpy)
        liftEffect $ FSM.pushAll push actionsList

        handlerCalled <- liftEffect $ Spy.get handlerSpy
        handlerCalled `shouldEqual` true

        liftEffect stop

      it "it is possible to handle actions" do
        handlerSpy <- liftEffect $ Spy.wasCalled

        let actionsList = Actions.init

        { push, stop } <- liftEffect
            $ FSM.run' sequencer (pure network) (Spy.with handlerSpy)
        liftEffect $ FSM.pushAll push actionsList

        liftEffect $ Spy.reset handlerSpy
        liftEffect $ push $ R.addPatch "foo"
        handlerCalled <- liftEffect $ Spy.get handlerSpy
        handlerCalled `shouldEqual` true

        liftEffect stop

      it "result is containing the network when actions are successful" do
        let
          actionsList =
                Actions.init
                    </> R.addPatch "foo"

        result /\ { stop } <- liftEffect
            $ FSM.fold sequencer (pure network) actionsList

        _ <- getOrFail result

        liftEffect stop

      it "result is containing the error when it happened" do
        let
          actionsList =
                Actions.init
                    </> R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists

        result /\ { stop } <- liftEffect
            $ FSM.fold sequencer (pure network) actionsList

        failIfNoError "no error" result

        liftEffect stop

      pending' "result is containing the last success, even if some error happened" do
        let
          actionsList =
                Actions.init
                    </> R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists
                    </> R.addPatch "bar"

        result /\ { stop } <- liftEffect
            -- $ FSM.runTracing toolkit network (Spy.with handlerSpy) actionsList
            $ FSM.fold sequencer (pure network) actionsList

        _ <- getOrFail result

        liftEffect stop

      it "result contains the error happened last, even if there were successes after" do
        let
          actionsList =
                Actions.init
                    </> R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists
                    </> R.addPatch "bar"

        result /\ { stop } <- liftEffect
            -- $ FSM.runTracing toolkit network (Spy.with handlerSpy) actionsList
            $ FSM.fold sequencer (pure network) actionsList

        failIfNoError "no error" result

        liftEffect stop

      pending "result contains all the errors happened"

    describe "pushing several actions" do

      it "pushing several actions performs them" do
        handlerSpy <- liftEffect $ Spy.wasCalled
        pushAllSpy <- liftEffect $ Spy.wasCalled

        let

          actionsList =
                Actions.init

        { push, stop } <- liftEffect
            $ FSM.run' sequencer (pure network) $ Spy.with handlerSpy
        liftEffect $ FSM.pushAll push actionsList

        liftEffect $ Spy.reset handlerSpy
        liftEffect $ Actions.pushAll (Spy.with pushAllSpy)
            $ Actions.init
                </> R.addPatch "foo"
                </> R.addPatch "bar"
        handlerCalled <- liftEffect $ Spy.get handlerSpy
        handlerCalled `shouldEqual` false
        pushHandlerCalled <- liftEffect $ Spy.get pushAllSpy
        pushHandlerCalled `shouldEqual` true

        liftEffect stop
