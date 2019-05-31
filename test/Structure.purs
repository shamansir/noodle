module RpdTest.Structure
    ( spec ) where

import Prelude

import Data.Lens (view) as L
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq

import Effect.Class (liftEffect)
import Effect.Class.Console (log)

import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual, fail)

import Rpd (init, run') as R
import Rpd.API as R
import Rpd.API ((</>))
import Rpd.Path
import Rpd.Optics as L
import Rpd.Network as R

import RpdTest.Util (withRpd)


data MyData
  = Bang

type MyRpd = R.Rpd (R.Network MyData)

myRpd :: MyRpd
myRpd =
  R.init "f"

spec :: Spec Unit
spec =
  describe "structure" do

    it "constructing the empty network works" do
      -- FIXME: fail on error
      _ <- liftEffect $ R.run' (log <<< show) (const $ pure unit) myRpd
      pure unit

    describe "order of addition" do

      it "adding nodes to the patch preserves the order of addition" $ do
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "one"
          </> R.addNode (toPatch "patch") "two"
          </> R.addNode (toPatch "patch") "three"
           #  withRpd \nw -> do
                case L.view (L._patchNodesByPath $ toPatch "patch") nw of
                  Just nodes ->
                    (nodes
                      <#> \(R.Node _ (ToNode { node }) _ _) -> node)
                      # Seq.toUnfoldable
                      # shouldEqual [ "one", "two", "three" ]
                  Nothing -> fail "patch wasn't found"

      it "adding inlets to the node preserves the order of addition" $ do
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node"
          </> R.addInlet (toNode "patch" "node") "one"
          </> R.addInlet (toNode "patch" "node") "two"
          </> R.addInlet (toNode "patch" "node") "three"
           #  withRpd \nw -> do
                case L.view (L._nodeInletsByPath $ toNode "patch" "node") nw of
                  Just inlets ->
                    (inlets
                      <#> \(R.Inlet _ (ToInlet { inlet }) _) -> inlet)
                      # Seq.toUnfoldable
                      # shouldEqual [ "one", "two", "three" ]
                  Nothing -> fail "node wasn't found"

      it "adding outlets to the node preserves the order of addition" $ do
        R.init "network"
          </> R.addPatch "patch"
          </> R.addNode (toPatch "patch") "node"
          </> R.addOutlet (toNode "patch" "node") "one"
          </> R.addOutlet (toNode "patch" "node") "two"
          </> R.addOutlet (toNode "patch" "node") "three"
           #  withRpd \nw -> do
                case L.view (L._nodeOutletsByPath $ toNode "patch" "node") nw of
                  Just inlets ->
                    (inlets
                      <#> \(R.Outlet _ (ToOutlet { outlet }) _) -> outlet)
                      # Seq.toUnfoldable
                      # shouldEqual [ "one", "two", "three" ]
                  Nothing -> fail "node wasn't found"
