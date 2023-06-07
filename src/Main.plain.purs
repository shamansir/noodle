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

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Console (log)

import Noodle.Network2 (Network)
import Noodle.Network2 as Network
import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3 as Toolkit
import Noodle.Patch4 (Patch)
import Noodle.Patch4 as Patch
import Noodle.Node2.MapsFolds.Flatten (NodeLineRec)
import Noodle.Node2.MapsFolds.Repr (class HasRepr)
import Noodle.Node2.MapsFolds.Repr (Repr(..)) as NMF
import Noodle.Patch4.MapsFolds.Repr as PMF
-- import Test.Repr.Patch4 (MyRepr)
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Id (Family(..), Family') as Node
import Noodle.Id (NodeId, class HasInputsAt, class HasOutputsAt)
import Noodle.Id (familyOf, reflectFamily') as Id

import Toolkit.Test (toolkit)
import Toolkit.Test (Instances) as TestToolkit

_foo = (Node.Family :: Node.Family "foo")
_bar = (Node.Family :: Node.Family "bar")
_sum = (Node.Family :: Node.Family "sum")


app gstate nw =
  { toolkit
  -- , components
  , currentPatch : Nothing -- Just "hydra"
  , network : nw
  , patchState : gstate
  }

data AlwaysUnitRepr = Unit_


instance Show AlwaysUnitRepr
    where
        show Unit_ = "Unit"


instance HasRepr Unit AlwaysUnitRepr where toRepr _ _ = Unit_
instance HasRepr String AlwaysUnitRepr where toRepr _ _ = Unit_
instance HasRepr Int AlwaysUnitRepr where toRepr _ _ = Unit_
instance HasRepr Boolean AlwaysUnitRepr where toRepr _ _ = Unit_


renderNode' :: forall m f is irl os orl. HasInputsAt is irl => HasOutputsAt os orl => MonadEffect m => NodeLineRec f AlwaysUnitRepr is os -> m Unit
renderNode' (nodeId /\ state /\ is /\ os) = liftEffect $ do
  log $ Id.reflectFamily' $ Id.familyOf nodeId
  log ">= inputs"
  traverse_ log $ Record.keys is
  log "<= outputs"
  traverse_ log $ Record.keys os
  log "--------"



-- renderNode :: forall f (is :: Row Type) irl (os :: Row Type) orl. HasInputsAt is irl => HasInputsAt os orl => Node f Unit is os Effect -> Effect Unit
renderNode :: forall f is os m. MonadEffect m => Node f Unit is os m -> m Unit
renderNode node = liftEffect $ do
  log $ Id.reflectFamily' $ Id.familyOf $ Node.id node
  -- log $ show $ Node.shape node


main :: Effect Unit
main = do
  nodeA <- Toolkit.spawn toolkit _foo
  nodeB <- Toolkit.spawn toolkit _bar
  nodeC <- Toolkit.spawn toolkit _bar
  nodeD <- Toolkit.spawn toolkit _sum

  let
    patch :: Patch Unit (TestToolkit.Instances Effect)
    patch = Patch.init toolkit
                # Patch.registerNode nodeA
                # Patch.registerNode nodeB
                # Patch.registerNode nodeC
                # Patch.registerNode nodeD
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
  sumReprs <- Record.get (proxify _sum) reprMap
  --   -- Patch.nodes patch

  log "families"
  traverse_ log (reflect' <$> families)

  log "==========="
  log "nodes 2"
  traverse_ renderNode' fooReprs
  traverse_ renderNode' barReprs
  traverse_ renderNode' sumReprs

  liftEffect $ log "🍝"
