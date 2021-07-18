module Noodle.Node.Shape where


import Noodle.Channel.Shape as Channel


import Prelude ((#), (<$>), (>>>), (<<<), ($))

import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (snoc)
import Data.Functor (class Functor)
import Data.Functor.Invariant (class Invariant)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, class Newtype)


type Inlets d = (String /-> (Channel.Shape d d)) -- forall a. (String /-> Channel.Shape a d)
type Outlets d = (String /-> (Channel.Shape d d)) -- forall a. (String /-> Channel.Shape a d)


newtype Shape d = Shape (Inlets d /\ Outlets d)


derive instance Newtype (Shape a) _


instance invariantShape :: Invariant Shape where
    imap :: forall a b. (a -> b) -> (b -> a) -> Shape a -> Shape b
    imap = move


empty :: forall d. Shape d
empty = Shape $ noInlets /\ noOutlets


make :: forall d. Inlets d -> Outlets d -> Shape d
make = Tuple.curry Shape


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
    -> String /\ (Channel.Shape d d)
    -> Inlets d
andInlet inlets (name /\ shape) =
    inlets # Map.insert name shape


andOutlet
    :: forall d
     . Outlets d
    -> String /\ (Channel.Shape d d)
    -> Outlets d
andOutlet outlets (name /\ shape) =
    outlets # Map.insert name shape


inlet :: forall d. String -> Shape d -> Maybe (Channel.Shape d d)
inlet name (Shape (inlets /\ _)) = Map.lookup name inlets


outlet :: forall d. String -> Shape d -> Maybe (Channel.Shape d d)
outlet name (Shape (_ /\ outlets)) = Map.lookup name outlets


inlets :: forall d. Shape d -> Array (String /\ (Channel.Shape d d))
inlets = unwrap >>> Tuple.fst >>> Map.toUnfoldable


outlets :: forall d. Shape d -> Array (String /\ (Channel.Shape d d))
outlets = unwrap >>> Tuple.snd >>> Map.toUnfoldable


dimensions :: forall d. Shape d -> Int /\ Int
dimensions (Shape (inlets /\ outlets)) = Map.size inlets /\ Map.size outlets


move :: forall a b. (a -> b) -> (b -> a) -> Shape a -> Shape b
move f g (Shape (inlets /\ outlets)) =
    Shape
        $ (Channel.move f g <$> inlets)
        /\ (Channel.move f g <$> outlets)