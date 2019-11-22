module Rpd.Test.Spec.Actions
    ( spec ) where

import Prelude

import Data.Either (Either(..))
import Data.Lens (view) as L
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Data.Tuple.Nested ((/\))

import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Aff (Aff, launchAff_)
import Effect.Ref as Ref
import Effect.Console (log) as Console

import FRP.Event as E

import Test.Spec (Spec, describe, it, pending, pending')
import Test.Spec.Assertions (shouldEqual, fail)

import Rpd.Test.Util.Either (getOrFail)

import Rpd.API.Action.Sequence ((</>))
import Rpd.API.Action.Sequence as Actions
import Rpd.API.Action.Sequence (addPatch, addNode, addInlet, addOutlet) as R
import Rpd.Path as P
import Rpd.Optics (_nodeInletsByPath, _nodeOutletsByPath, _patchNodesByPath) as L
import Rpd.Network (Inlet(..), Network, Node(..), Outlet(..)) as R
import Rpd.Network (empty) as N
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

    it "prepare" do
      pure unit

    describe "running" do

        it "handler is called with the model on performed actions" do
            handlerCalledRef <- liftEffect $ Ref.new false

            let

                actions =
                    Actions.init
                        </> R.addPatch "foo"
                everyStep _ = Ref.write true handlerCalledRef

            _ <- liftEffect
                $ Actions.run toolkit network everyStep actions

            handlerCalled <- liftEffect $ Ref.read handlerCalledRef
            handlerCalled `shouldEqual` true

            pure unit

        it "handler receives error when it happened" do
            errHandlerCalledRef <- liftEffect $ Ref.new false

            let

                actions =
                    Actions.init
                        </> R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists
                everyStep (Left error) = Ref.write true errHandlerCalledRef
                everyStep (Right model) = pure unit

            _ <- liftEffect
                $ Actions.run toolkit network everyStep actions

            handlerCalled <- liftEffect $ Ref.read errHandlerCalledRef
            handlerCalled `shouldEqual` true

            pure unit

        pending' "when error happened, next models still arrive" do
            errHandlerCalledRef <- liftEffect $ Ref.new false
            modelHandlerCalledRef <- liftEffect $ Ref.new false

            let

                actions = Actions.init
                everyStep (Left error) = Ref.write true errHandlerCalledRef
                everyStep (Right model) = Ref.write true modelHandlerCalledRef

            { pushAction, stop } <- liftEffect
                $ Actions.run toolkit network everyStep actions

            liftEffect $ pushAction
                $ R.addNode (P.toPatch "foo") "fail" Node -- no such patch exists

            errHandlerCalled <- liftEffect $ Ref.read errHandlerCalledRef
            errHandlerCalled `shouldEqual` true

            liftEffect $ Ref.write false modelHandlerCalledRef
            liftEffect $ pushAction $ R.addPatch "foo"
            modelHandlerCalled <- liftEffect $ Ref.read modelHandlerCalledRef
            modelHandlerCalled `shouldEqual` true

            pure unit

        it "stopping stops sending model updates" do
            handlerCalledRef <- liftEffect $ Ref.new false

            let

                actions =
                    Actions.init
                       </> R.addPatch "foo"
                everyStep _ = Ref.modify_ (const true) handlerCalledRef

            { pushAction, stop } <- liftEffect
                $ Actions.run toolkit network everyStep actions

            liftEffect $ Ref.write false handlerCalledRef
            liftEffect $ pushAction $ R.addPatch "bar"
            handlerCalled <- liftEffect $ Ref.read handlerCalledRef
            handlerCalled `shouldEqual` true

            liftEffect stop
            liftEffect $ Ref.write false handlerCalledRef
            liftEffect $ pushAction $ R.addPatch "buz"
            handlerCalled' <- liftEffect $ Ref.read handlerCalledRef
            handlerCalled' `shouldEqual` false

            pure unit

        pending' "it is possible to subscribe to `actions` flow" do
            pure unit

    describe "folding" do

        it "aaa" do
            pure unit

        it "bbb" do
            pure unit

    describe "tracing" do

        it "aaa" do
            pure unit

        it "bbb" do
            pure unit

    {-
    describe "order of addition" do

      it "adding nodes to the patch preserves the order of addition" $ do
        result /\ _ <-
            liftEffect
            $ Actions.runFolding toolkit network
            $ Actions.init
                </> R.addPatch "patch"
                </> R.addNode (P.toPatch "patch") "one" Node
                </> R.addNode (P.toPatch "patch") "two" Node
                </> R.addNode (P.toPatch "patch") "three" Node
        network' <- getOrFail result network
        case L.view (L._patchNodesByPath $ P.toPatch "patch") network' of
          Just nodes ->
            (nodes
              <#> \(R.Node _ (P.ToNode { node }) _ _ _) -> node)
              # Seq.toUnfoldable
              # shouldEqual [ "one", "two", "three" ]
          Nothing -> fail "patch wasn't found"
        pure unit

      it "adding inlets to the node preserves the order of addition" $ do
        result /\ _<-
            liftEffect
            $ Actions.runFolding toolkit network
            $ Actions.init
              </> R.addPatch "patch"
              </> R.addNode (P.toPatch "patch") "node" Node
              </> R.addInlet (P.toNode "patch" "node") "one" Channel
              </> R.addInlet (P.toNode "patch" "node") "two" Channel
              </> R.addInlet (P.toNode "patch" "node") "three" Channel
        network' <- getOrFail result network
        case L.view (L._nodeInletsByPath $ P.toNode "patch" "node") network' of
          Just inlets ->
            (inlets
              <#> \(R.Inlet _ (P.ToInlet { inlet }) _ _) -> inlet)
              # Seq.toUnfoldable
              # shouldEqual [ "one", "two", "three" ]
          Nothing -> fail "node wasn't found"
        pure unit


      it "adding outlets to the node preserves the order of addition" $ do
        result /\ _ <-
            liftEffect
            $ Actions.runFolding toolkit network
            $ Actions.init
              </> R.addPatch "patch"
              </> R.addNode (P.toPatch "patch") "node" Node
              </> R.addOutlet (P.toNode "patch" "node") "one" Channel
              </> R.addOutlet (P.toNode "patch" "node") "two" Channel
              </> R.addOutlet (P.toNode "patch" "node") "three" Channel
        network' <- getOrFail result network
        case L.view (L._nodeOutletsByPath $ P.toNode "patch" "node") network' of
          Just inlets ->
            (inlets
              <#> \(R.Outlet _ (P.ToOutlet { outlet }) _ _) -> outlet)
              # Seq.toUnfoldable
              # shouldEqual [ "one", "two", "three" ]
          Nothing -> fail "node wasn't found"
        pure unit

      -- TODO: subPatches
    -}
