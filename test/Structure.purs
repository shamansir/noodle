module RpdTest.Structure
    ( spec ) where

import Prelude

import Data.Lens (view) as L
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq

import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Aff (Aff, launchAff_)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)

import RpdTest.Helper (withRpd, withRpd_)

import Rpd.API.Action.Sequence (LastStep(..), (</>))
import Rpd.API.Action.Sequence as Actions
import Rpd.API.Action.Sequence (init, addPatch, addNode, addInlet, addOutlet) as R
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

    it "constructing the empty network works" do
      withRpd toolkit network R.init Actions.pass

    describe "order of addition" do

      it "adding nodes to the patch preserves the order of addition" $ do
        withRpd_ toolkit network
            (R.init
                </> R.addPatch "patch"
                </> R.addNode (P.toPatch "patch") "one" Node
                </> R.addNode (P.toPatch "patch") "two" Node
                </> R.addNode (P.toPatch "patch") "three" Node)
            (LastStep $ \nw -> do
                launchAff_ $ case L.view (L._patchNodesByPath $ P.toPatch "patch") nw of
                  Just nodes ->
                    (nodes
                      <#> \(R.Node _ (P.ToNode { node }) _ _ _) -> node)
                      # Seq.toUnfoldable
                      # shouldEqual [ "one", "two", "three" ]
                  Nothing -> fail "patch wasn't found")

      it "adding inlets to the node preserves the order of addition" $ do
        withRpd_ toolkit network
          (R.init
            </> R.addPatch "patch"
            </> R.addNode (P.toPatch "patch") "node" Node
            </> R.addInlet (P.toNode "patch" "node") "one" Channel
            </> R.addInlet (P.toNode "patch" "node") "two" Channel
            </> R.addInlet (P.toNode "patch" "node") "three" Channel)
           (LastStep $ \nw -> do
                launchAff_ $ case L.view (L._nodeInletsByPath $ P.toNode "patch" "node") nw of
                  Just inlets ->
                    (inlets
                      <#> \(R.Inlet _ (P.ToInlet { inlet }) _ _) -> inlet)
                      # Seq.toUnfoldable
                      # shouldEqual [ "one", "two", "three" ]
                  Nothing -> fail "node wasn't found")

      it "adding outlets to the node preserves the order of addition" $ do
        withRpd_ toolkit network
          (R.init
            </> R.addPatch "patch"
            </> R.addNode (P.toPatch "patch") "node" Node
            </> R.addOutlet (P.toNode "patch" "node") "one" Channel
            </> R.addOutlet (P.toNode "patch" "node") "two" Channel
            </> R.addOutlet (P.toNode "patch" "node") "three" Channel)
           (LastStep $ \nw -> do
                launchAff_ $ case L.view (L._nodeOutletsByPath $ P.toNode "patch" "node") nw of
                  Just inlets ->
                    (inlets
                      <#> \(R.Outlet _ (P.ToOutlet { outlet }) _ _) -> outlet)
                      # Seq.toUnfoldable
                      # shouldEqual [ "one", "two", "three" ]
                  Nothing -> fail "node wasn't found")

      -- TODO: subPatches
