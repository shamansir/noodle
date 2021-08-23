module Noodle.Node.Shape where


import Noodle.Channel.Shape as Channel


import Prelude

import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array as Array

import Data.Functor (class Functor)
import Data.Functor.Invariant (class Invariant)
import Data.Maybe (Maybe)

import Data.Newtype (unwrap, class Newtype)



type InletId = String
type OutletId = String


type Inlets d = Array (InletId /\ (Channel.Shape d d)) -- forall a. (String /-> Channel.Shape a d)
type Outlets d = Array (OutletId /\ (Channel.Shape d d)) -- forall a. (String /-> Channel.Shape a d)


newtype Shape d = Shape (Inlets d /\ Outlets d)


derive instance Newtype (Shape d) _


instance invariantShape :: Invariant Shape where
    imap :: forall a b. (a -> b) -> (b -> a) -> Shape a -> Shape b
    imap = move


empty :: forall d. Shape d
empty = Shape $ noInlets /\ noOutlets


make :: forall d. Inlets d -> Outlets d -> Shape d
make = Tuple.curry Shape


noInlets :: forall d. Inlets d
noInlets = []


noOutlets :: forall d. Outlets d
noOutlets = []


formInlets :: forall d. Array (InletId /\ Channel.Shape d d) -> Inlets d
formInlets = identity


formOutlets :: forall d. Array (OutletId /\ Channel.Shape d d) -> Outlets d
formOutlets = identity


infixl 1 andInlet as ~<

infixl 1 andOutlet as >~


withInlets :: forall d. Inlets d
withInlets = []


withOutlets :: forall d. Outlets d
withOutlets = []


andInlet
    :: forall d
     . Inlets d
    -> InletId /\ (Channel.Shape d d)
    -> Inlets d
andInlet = Array.snoc


andOutlet
    :: forall d
     . Outlets d
    -> OutletId /\ (Channel.Shape d d)
    -> Outlets d
andOutlet = Array.snoc


inlet :: forall d. InletId -> Shape d -> Maybe (Channel.Shape d d)
inlet name (Shape (inlets /\ _)) =
    Tuple.snd <$> Array.find (Tuple.fst >>> (==) name) inlets


outlet :: forall d. OutletId -> Shape d -> Maybe (Channel.Shape d d)
outlet name (Shape (_ /\ outlets)) =
    Tuple.snd <$> Array.find (Tuple.fst >>> (==) name) outlets


inlets :: forall d. Shape d -> Array (InletId /\ Channel.Shape d d)
inlets = unwrap >>> Tuple.fst


inletsBy :: forall d. (Channel.Shape d d -> Boolean) -> Shape d -> Array (InletId /\ Channel.Shape d d)
inletsBy pred = inlets >>> Array.filter (Tuple.snd >>> pred)


outlets :: forall d. Shape d -> Array (OutletId /\ Channel.Shape d d)
outlets = unwrap >>> Tuple.snd


outletsBy :: forall d. (Channel.Shape d d -> Boolean) -> Shape d -> Array (InletId /\ Channel.Shape d d)
outletsBy pred = outlets >>> Array.filter (Tuple.snd >>> pred)


dimensions :: forall d. Shape d -> Int /\ Int
dimensions (Shape (inlets /\ outlets)) = Array.length inlets /\ Array.length outlets


dimensionsBy :: forall d. (Channel.Shape d d -> Boolean) -> Shape d -> Int /\ Int
dimensionsBy pred (Shape (inlets /\ outlets)) =
    (Array.length $ Array.filter (Tuple.snd >>> pred) inlets)
    /\
    (Array.length $ Array.filter (Tuple.snd >>> pred) outlets)


move :: forall a b. (a -> b) -> (b -> a) -> Shape a -> Shape b
move f g (Shape (inlets /\ outlets)) =
    Shape
        $ (map (Channel.move f g) <$> inlets)
        /\ (map (Channel.move f g) <$> outlets)