module Rpd.Test.Spec.Actions
    ( spec ) where

import Prelude

import Debug.Trace as DT

import Data.Either (Either(..))
import Data.Lens (view) as L
import Data.Maybe (Maybe(..), isJust)
import Data.Sequence as Seq
import Data.Array (length)
import Data.Tuple (fst, snd) as Tuple
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

import Rpd.API.Action.Sequence ((</>))
import Rpd.API.Action.Sequence (addPatch, addNode, addInlet, addOutlet) as R
import Rpd.API.Action.Sequence as Actions

import Rpd.Network (Inlet(..), Network, Node(..), Outlet(..)) as R
import Rpd.Network (empty) as N
import Rpd.Optics (_nodeInletsByPath, _nodeOutletsByPath, _patchNodesByPath, _patchByPath) as L
import Rpd.Path as P
import Rpd.Test.Util.Actions (getOrFail, failIfNoErrors)
import Rpd.Test.Util.Spy as Spy
import Rpd.Test.Util.Assertions
import Rpd.Toolkit as T


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

    it "init" do
      result /\ _ <- liftEffect
        $ Actions.runFolding toolkit network Actions.init
      _ <- getOrFail result network
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

          { stop } <- liftEffect
              $ Actions.run toolkit network (Spy.with handlerSpy) actionsList

          handlerCalled <- liftEffect $ Spy.get handlerSpy
          handlerCalled `shouldEqual` true

          liftEffect stop

      it "handler receives error when it happened" do
          handlerSpy <- liftEffect $ Spy.ifErrors

          let

              actionsList =
                  Actions.init
                      </> R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists

          { stop } <- liftEffect
              $ Actions.run toolkit network (Spy.with handlerSpy) actionsList

          handlerCalled <- liftEffect $  Spy.get handlerSpy
          handlerCalled `shouldEqual` true

          liftEffect stop

      it "pushing actions works propetly" do
          handlerSpy <- liftEffect $ Spy.wasCalled

          let

            actionsList =
                  Actions.init
                      </> R.addPatch "foo"

          { pushAction, stop } <- liftEffect
              $ Actions.run toolkit network (Spy.with handlerSpy) actionsList

          liftEffect $ Spy.reset handlerSpy
          liftEffect $ pushAction $ R.addPatch "bar"
          handlerCalled <- liftEffect $ Spy.get handlerSpy
          handlerCalled `shouldEqual` true

          liftEffect stop

      it "when error happened, next models still arrive" do
          handlerSpy <- liftEffect $ Spy.wasCalled
          errHandlerSpy <- liftEffect $ Spy.ifErrors

          let

              actionsList = Actions.init
              everyStep v
                  =  Spy.consider handlerSpy v
                  <> Spy.consider errHandlerSpy v

          { pushAction, stop } <- liftEffect
              $ Actions.run toolkit network everyStep actionsList

          liftEffect $ pushAction
              $ R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists

          errHandlerCalled <- liftEffect $ Spy.get errHandlerSpy
          errHandlerCalled `shouldEqual` true

          liftEffect $ Spy.reset handlerSpy
          liftEffect $ pushAction $ R.addPatch "foo"
          handlerCalled <- liftEffect $ Spy.get handlerSpy
          handlerCalled `shouldEqual` true

          liftEffect stop

      it "when some error happened, it is reported only once" do
          traceSpy <- liftEffect Spy.last

          let
              actionsList = Actions.init
              collectErrorsSpy = Spy.contramap Tuple.fst traceSpy

          { pushAction, stop } <- liftEffect
              $ Actions.run toolkit network (Spy.with collectErrorsSpy) actionsList

          liftEffect $ pushAction
              $ R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists

          errorsBefore <- liftEffect $ Spy.get collectErrorsSpy
          shouldHaveValue errorsBefore
          --liftEffect $ Spy.reset collectErrorsSpy
          -- liftEffect $ pushAction $ R.addNode (P.toPatch "foo") "fail" Node
          liftEffect $ pushAction $ R.addPatch "foo"
          errorsNow <- liftEffect $ Spy.get collectErrorsSpy
          errorsNow `shouldEqual` errorsBefore

          liftEffect stop

      it "the system recovers from errors" do
          lastSpy <- liftEffect Spy.last

          let
              actionsList = Actions.init
              lastNetworkSpy = Spy.contramap Tuple.snd lastSpy

          { pushAction, stop } <- liftEffect
              $ Actions.run toolkit network (Spy.with lastNetworkSpy) actionsList

          liftEffect $ pushAction
              $ R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists

          liftEffect $ pushAction $ R.addPatch "foo"
          maybeLastNetwork <- liftEffect $ Spy.get lastNetworkSpy
          shouldHaveValue maybeLastNetwork
          shouldHaveValue
              (
                maybeLastNetwork
                  >>= L.view (L._patchByPath $ P.toPatch "foo")
              )

          liftEffect stop

      it "stopping stops sending model updates" do
          handlerSpy <- liftEffect $ Spy.wasCalled

          let

            actionsList =
                  Actions.init
                      </> R.addPatch "foo"

          { pushAction, stop } <- liftEffect
              $ Actions.run toolkit network (Spy.with handlerSpy) actionsList

          liftEffect $ Spy.reset handlerSpy
          liftEffect $ pushAction $ R.addPatch "bar"
          handlerCalled <- liftEffect $ Spy.get handlerSpy
          handlerCalled `shouldEqual` true

          liftEffect stop
          liftEffect $ Spy.reset handlerSpy
          liftEffect $ pushAction $ R.addPatch "buz"
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
            $ Actions.runFolding toolkit network actionsList

        _ <- getOrFail result network

        liftEffect stop

      it "result is containing the error when it happened" do
        let
          actionsList =
                Actions.init
                    </> R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists

        result /\ { stop } <- liftEffect
            $ Actions.runFolding toolkit network actionsList

        failIfNoErrors "no error" result

        liftEffect stop

      pending' "result is containing the last success, even if some error happened" do
        let
          actionsList =
                Actions.init
                    </> R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists
                    </> R.addPatch "bar"

        result /\ { stop } <- liftEffect
            $ Actions.runFolding toolkit network actionsList

        _ <- getOrFail result network

        liftEffect stop

      it "result contains the error happened last, even if there were successes after" do
        let
          actionsList =
                Actions.init
                    </> R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists
                    </> R.addPatch "bar"

        result /\ { stop } <- liftEffect
            $ Actions.runFolding toolkit network actionsList

        failIfNoErrors "no error" result

        liftEffect stop

      pending "result contains all the errors happened"

      pending' "pushing actions works propetly" do
        handlerSpy <- liftEffect $ Spy.wasCalled

        let

          actionsList =
                Actions.init
                    </> R.addPatch "foo"

        -- nothing to listen for

        _ /\ { pushAction, stop } <- liftEffect
            $ Actions.runFolding toolkit network actionsList

        liftEffect $ Spy.reset handlerSpy
        liftEffect $ pushAction $ R.addPatch "bar"
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

        _ /\{ pushAction, stop } <- liftEffect
            $ Actions.runTracing toolkit network (Spy.with handlerSpy) actionsList

        handlerCalled <- liftEffect $ Spy.get handlerSpy
        handlerCalled `shouldEqual` true

        liftEffect stop

      it "it is possible to handle actions" do
        handlerSpy <- liftEffect $ Spy.wasCalled

        let actionsList = Actions.init

        _ /\{ pushAction, stop } <- liftEffect
            $ Actions.runTracing toolkit network (Spy.with handlerSpy) actionsList

        liftEffect $ Spy.reset handlerSpy
        liftEffect $ pushAction $ R.addPatch "foo"
        handlerCalled <- liftEffect $ Spy.get handlerSpy
        handlerCalled `shouldEqual` true

        liftEffect stop

      it "result is containing the network when actions are successful" do
        let
          actionsList =
                Actions.init
                    </> R.addPatch "foo"

        result /\ { stop } <- liftEffect
            $ Actions.runTracing toolkit network (const $ pure unit) actionsList

        _ <- getOrFail result network

        liftEffect stop

      it "result is containing the error when it happened" do
        let
          actionsList =
                Actions.init
                    </> R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists

        result /\ { stop } <- liftEffect
            $ Actions.runTracing toolkit network (const $ pure unit) actionsList

        failIfNoErrors "no error" result

        liftEffect stop

      pending' "result is containing the last success, even if some error happened" do
        let
          actionsList =
                Actions.init
                    </> R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists
                    </> R.addPatch "bar"

        result /\ { stop } <- liftEffect
            $ Actions.runTracing toolkit network (const $ pure unit) actionsList

        _ <- getOrFail result network

        liftEffect stop

      it "result contains the error happened last, even if there were successes after" do
        let
          actionsList =
                Actions.init
                    </> R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists
                    </> R.addPatch "bar"

        result /\ { stop } <- liftEffect
            $ Actions.runTracing toolkit network (const $ pure unit) actionsList

        failIfNoErrors "no error" result

        liftEffect stop

      pending "result contains all the errors happened"

    describe "pushing several actions" do

      it "pushing several actions performs them" do
        handlerSpy <- liftEffect $ Spy.wasCalled
        pushAllSpy <- liftEffect $ Spy.wasCalled

        let

          actionsList =
                Actions.init

        { pushAction, stop } <- liftEffect
            $ Actions.run toolkit network (Spy.with handlerSpy) actionsList

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
