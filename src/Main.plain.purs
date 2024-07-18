module MainPlain where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple.Nested ((/\), type (/\))
import Data.SProxy (proxify, reflect')
import Type.Proxy (Proxy(..))
import Record as Record
import Record.Extra as Record
import Prim.RowList as RL
import Data.Repr as R

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Console (log)

import Noodle.Network (Network)
import Noodle.Network as Network
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit as Toolkit
import Noodle.Patch (Patch)
import Noodle.Patch as Patch
import Noodle.Node.MapsFolds.Flatten (NodeLineRec)
import Noodle.Node.MapsFolds.Repr (class HasRepr)
import Noodle.Node.MapsFolds.Repr (Repr(..)) as NMF
import Noodle.Patch.MapsFolds.Repr as PMF
-- import Test.Repr.Patch4 (MyRepr)
import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Id (Family(..), Family') as Node
import Noodle.Id (NodeId, class HasInputsAt, class HasOutputsAt)
import Noodle.Id (familyOf, reflectFamily') as Id

import Toolkit.Test (toolkit) as Test
import Toolkit.Test (TestToolkit)
import Toolkit.Test (Instances, Families) as TestToolkit
import Toolkit.Test (FooNode, BarNode, SumNode, ConcatNode) as T
import Toolkit.Test.Repr (AlwaysUnitRepr)


_foo = (Node.Family :: _ "foo")
_bar = (Node.Family :: _ "bar")
_concat = (Node.Family :: _ "concat")
_sum = (Node.Family :: _ "sum")


renderNode' :: forall m f is irl os orl. HasInputsAt is irl => HasOutputsAt os orl => MonadEffect m => NodeLineRec f AlwaysUnitRepr is os -> m Unit
renderNode' (nodeId /\ state /\ is /\ os) = liftEffect $ do
  log $ Id.reflectFamily' $ Id.familyOf nodeId
  log ">= inputs"
  traverse_ log $ Record.keys is
  log "<= outputs"
  traverse_ log $ Record.keys os
  log "--------"



-- renderNode :: forall f (is :: Row Type) irl (os :: Row Type) orl. HasInputsAt is irl => HasInputsAt os orl => Node f Unit is os Effect -> Effect Unit
renderNode :: forall f is os m. MonadEffect m => Node f Unit is os AlwaysUnitRepr m -> m Unit
renderNode node = liftEffect $ do
  log $ Id.reflectFamily' $ Id.familyOf $ Node.id node
  -- log $ show $ Node.shape node


main :: Effect Unit
main = do
  let
    toolkit :: TestToolkit AlwaysUnitRepr Effect
    toolkit = Test.toolkit

  nodeA <- Toolkit.spawn toolkit _foo
  nodeB <- Toolkit.spawn toolkit _bar
  nodeC <- Toolkit.spawn toolkit _bar
  nodeD <- Toolkit.spawn toolkit _sum
  nodeE <- Toolkit.spawn toolkit _concat

  let
    patch = Patch.init toolkit
                # Patch.registerNode nodeA
                # Patch.registerNode nodeB
                # Patch.registerNode nodeC
                # Patch.registerNode nodeD
                # Patch.registerNode nodeE
    -- nw :: Network
    nw = Network.init toolkit
                # Network.addPatch "test" patch
    families = Toolkit.nodeFamilies toolkit
    reprMap =
          Patch.toRepr
              (Proxy :: Proxy Effect)
              (NMF.Repr :: NMF.Repr AlwaysUnitRepr)
              patch
    state = app unit nw

  fooReprs <- Record.get (proxify _foo) reprMap
  barReprs <- Record.get (proxify _bar) reprMap
  concatReprs <- Record.get (proxify _concat) reprMap
  sumReprs <- Record.get (proxify _sum) reprMap
  --   -- Patch.nodes patch

  log "families"
  traverse_ log (reflect' <$> families)

  log "==========="
  log "nodes 2"
  traverse_ renderNode' fooReprs
  traverse_ renderNode' barReprs
  traverse_ renderNode' concatReprs
  traverse_ renderNode' sumReprs

  liftEffect $ log "🍝"


type AppState gstate repr m =
  { toolkit :: TestToolkit repr m
  , currentPatch :: Maybe (Patch gstate (TestToolkit.Instances repr m))
  , network :: Network gstate (TestToolkit.Families repr m) (TestToolkit.Instances repr m)
  , patchState :: gstate
  }


app :: Unit -> _ -> AppState Unit AlwaysUnitRepr Effect
app gstate nw =
  { toolkit : Test.toolkit
  -- , components
  , currentPatch : Nothing -- Just "hydra"
  , network : nw
  , patchState : gstate
  }