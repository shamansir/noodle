module Noodle.Node.Shape where


import Noodle.Channel.Shape as Channel


import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (snoc)


type Inlets d = (String /-> Channel.Shape d)
type Outlets d = (String /-> Channel.Shape d)


type Shape d = Inlets d /\ Outlets d


empty :: forall d. Shape d
empty = noInlets /\ noOutlets


noInlets :: forall d. Inlets d
noInlets = Map.empty


noOutlets :: forall d. Outlets d
noOutlets = Map.empty


infixl 1 andInlet as ~<

infixl 1 andOutlet as >~


withInlets :: forall d. Array (String /\ Channel.Shape d) -> Inlets d
withInlets = Map.fromFoldable


withOutlets :: forall d. Array (String /\ Channel.Shape d) -> Outlets d
withOutlets = Map.fromFoldable


andInlet
    :: forall d
     . Array (String /\ Channel.Shape d)
    -> String /\ Channel.Shape d
    -> Array (String /\ Channel.Shape d)
andInlet inlets (name /\ shape) =
    inlets `snoc` (name /\ shape)


andOutlet
    :: forall d
     . Array (String /\ Channel.Shape d)
    -> String /\ Channel.Shape d
    -> Array (String /\ Channel.Shape d)
andOutlet outlets (name /\ shape) =
    outlets `snoc` (name /\ shape)
