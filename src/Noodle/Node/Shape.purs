module Noodle.Node.Shape where


import Noodle.Channel.Shape as Channel


import Prelude ((#))
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


withInlets :: forall d. Inlets d
withInlets = Map.empty


withOutlets :: forall d. Outlets d
withOutlets = Map.empty


andInlet
    :: forall d
     . Inlets d
    -> String /\ Channel.Shape d
    -> Inlets d
andInlet inlets (name /\ shape) =
    inlets # Map.insert name shape


andOutlet
    :: forall d
     . Outlets d
    -> String /\ Channel.Shape d
    -> Outlets d
andOutlet outlets (name /\ shape) =
    outlets # Map.insert name shape
