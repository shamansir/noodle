module Noodle.Test.Spec.Structure
    ( spec ) where

import Prelude

import Data.Lens (view) as L
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Data.Tuple.Nested ((/\))

import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Aff (Aff, launchAff_)

import Test.Spec (Spec, describe, it, pending, pending')
import Test.Spec.Assertions (shouldEqual, fail)

import Noodle.Test.Util.Actions (getOrFail)

import FSM (fold) as Actions
import Noodle.API.Action.Sequence ((</>))
import Noodle.API.Action.Sequence as Actions
import Noodle.API.Action.Sequence (addPatch, addNode, addInlet, addOutlet) as R
import Noodle.Path as P
import Noodle.Optics (_nodeInletsByPath, _nodeOutletsByPath, _patchNodesByPath) as L
import Noodle.Network (Inlet(..), Network, Node(..), Outlet(..)) as R
import Noodle.Network (empty) as N
import Noodle.Toolkit as T


data MyData
  = Bang

data Channel = Channel

data Node = Node


toolkit :: T.Toolkit MyData Channel Node
toolkit = T.empty "foo"


network :: R.Network MyData Channel Node
network = N.empty "foo"


sequencer :: Actions.Sequencer MyData Channel Node
sequencer = Actions.make toolkit


spec :: Spec Unit
spec =
  describe "structure" do

    it "constructing the empty network works" do
      result /\ _ <- liftEffect
        $ Actions.fold sequencer (pure network) Actions.init
      _ <- getOrFail result
      pure unit

    describe "order of addition" do

      it "adding nodes to the patch preserves the order of addition" $ do
        result /\ _ <-
            liftEffect
            $ Actions.fold sequencer (pure network)
            $ Actions.init
                </> R.addPatch "patch"
                </> R.addNode (P.toPatch "patch") "one" Node
                </> R.addNode (P.toPatch "patch") "two" Node
                </> R.addNode (P.toPatch "patch") "three" Node
        network' <- getOrFail result
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
            $ Actions.fold sequencer (pure network)
            $ Actions.init
              </> R.addPatch "patch"
              </> R.addNode (P.toPatch "patch") "node" Node
              </> R.addInlet (P.toNode "patch" "node") "one" Channel
              </> R.addInlet (P.toNode "patch" "node") "two" Channel
              </> R.addInlet (P.toNode "patch" "node") "three" Channel
        network' <- getOrFail result
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
            $ Actions.fold sequencer (pure network)
            $ Actions.init
              </> R.addPatch "patch"
              </> R.addNode (P.toPatch "patch") "node" Node
              </> R.addOutlet (P.toNode "patch" "node") "one" Channel
              </> R.addOutlet (P.toNode "patch" "node") "two" Channel
              </> R.addOutlet (P.toNode "patch" "node") "three" Channel
        network' <- getOrFail result
        case L.view (L._nodeOutletsByPath $ P.toNode "patch" "node") network' of
          Just inlets ->
            (inlets
              <#> \(R.Outlet _ (P.ToOutlet { outlet }) _ _) -> outlet)
              # Seq.toUnfoldable
              # shouldEqual [ "one", "two", "three" ]
          Nothing -> fail "node wasn't found"
        pure unit

      -- TODO: subPatches

      pending "disconnecting the inlet from outlet removes the corresponding link"

      pending "removing the node also removes the links which were connected to that node"
