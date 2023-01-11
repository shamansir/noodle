module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple.Nested ((/\), type (/\))
import Type.Proxy (Proxy(..))
import Record as Record

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Noodle.Network2 (Network)
import Noodle.Network2 as Network
import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3 as Toolkit
import Noodle.Patch4 (Patch)
import Noodle.Patch4 as Patch
import Noodle.Patch4.MapsFolds.Repr (class HasRepr, NodeLineRec)
import Noodle.Patch4.MapsFolds.Repr as PMF
import Noodle.Node2 (Node)
import Noodle.Node2 as Node
import Noodle.Id (Family(..), Family') as Node
import Noodle.Id (NodeId, class HasInputsAt)
import Noodle.Id (familyOf, reflectFamily') as Id
import Noodle.Id (reflect')

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

data MyRepr = Unit_


instance Show MyRepr
    where
        show Unit_ = "Unit"


instance HasRepr Unit MyRepr where toRepr _ _ = Unit_
instance HasRepr String MyRepr where toRepr _ _ = Unit_
instance HasRepr Int MyRepr where toRepr _ _ = Unit_
instance HasRepr Boolean MyRepr where toRepr _ _ = Unit_


{- type ReprInstances m =
    ( foo :: m (Array (NodeLineRec "foo" MyRepr ( foo :: MyRepr, bar :: MyRepr, c :: MyRepr ) ( out :: MyRepr )))
    , bar :: m (Array (NodeLineRec "bar" MyRepr ( a :: MyRepr, b :: MyRepr, c :: MyRepr ) ( x :: MyRepr )))
    , sum :: m (Array (NodeLineRec "sym" MyRepr ( a :: MyRepr, b :: MyRepr ) ( sum :: MyRepr )))
    ) -}


renderNode' :: forall f is os. NodeId f /\ MyRepr /\ Record is /\ Record os -> Effect Unit
renderNode' (nodeId /\ _ /\ _ /\ _) = log $ Id.reflectFamily' $ Id.familyOf nodeId


-- renderNode :: forall f (is :: Row Type) irl (os :: Row Type) orl. HasInputsAt is irl => HasInputsAt os orl => Node f Unit is os Effect -> Effect Unit
renderNode :: forall f is os. Node f Unit is os Effect -> Effect Unit
renderNode node = do
  log $ Id.reflectFamily' $ Id.familyOf $ Node.id node
  -- log $ show $ Node.shape node


main :: Effect Unit
main = do
  nodeA <- Toolkit.spawn toolkit _foo
  nodeB <- Toolkit.spawn toolkit _bar
  nodeC <- Toolkit.spawn toolkit _bar

  let
    patch :: Patch Unit (TestToolkit.Instances Effect)
    patch = Patch.init toolkit
                # Patch.registerNode nodeA
                # Patch.registerNode nodeB
                # Patch.registerNode nodeC
    nw = Network.init toolkit
                # Network.addPatch "test" patch
    families = Toolkit.nodeFamilies toolkit
    -- reprMap :: Record (ReprInstances Effect)
    -- reprMap =
    --       Patch.toRepr
    --           (Proxy :: Proxy Effect)
    --           (PMF.Repr :: PMF.Repr MyRepr)
    --           patch
    state = app unit nw

  -- fooReprs <- Record.get _foo reprMap
  -- barReprs <- Record.get _bar reprMap
  -- sumReprs <- Record.get _sum reprMap
  --   -- Patch.nodes patch

  traverse_ log (reflect' <$> families)

  traverse_ renderNode (Patch.nodes patch)

  -- traverse_ renderNode fooReprs
  -- traverse_ renderNode barReprs
  -- traverse_ renderNode sumReprs

  liftEffect $ log "🍝"




{-

{ foo :
    Unit_
    /\ { foo : String_ "aaa", bar : String_ "bbb", c : Int_ 32 }
    /\ { out : Bool_ false }
, bar :
    Unit_
    /\ { a : String_ "aaa", b : String_ "bbb", c : String_ "ccc" }
    /\ { x : Int_ 12 }
, sum :
    Unit_
    /\ { a : Int_ 40, b : Int_ 2 }
    /\ { sum : Int_ 42 }
}
-}