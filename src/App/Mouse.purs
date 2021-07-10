module App.Mouse where


import Prelude (class Show, show, (<>), ($))
import Data.Maybe (Maybe, maybe)
import Data.Tuple.Nested ((/\), type (/\))
import Halogen.Svg.Attributes (a)
import Web.UIEvent.MouseEvent as ME


data State a
    = Move ( Int /\ Int )
    | Dragging ( Int /\ Int ) ( Int /\ Int ) (Maybe a)
    | DropAt ( Int /\ Int ) (Maybe a)


init :: forall a. State a
init = Move ( 0 /\ 0 )


apply :: forall a. ((Int /\ Int) -> Maybe a) -> ME.MouseEvent -> State a -> State a
apply toItem event curState =
    analyze curState
    where
        analyze (Move (x /\ y)) =
            Move $ ME.clientX event /\ ME.clientY event
        analyze (Dragging (startX /\ startY) ( curX /\ curY ) maybeItem) =
            Move $ ME.clientX event /\ ME.clientY event
        analyze (DropAt ( curX /\ curY ) maybeItem) =
            Move $ ME.clientX event /\ ME.clientY event


instance showMouse :: Show a => Show (State a) where
    show (Move (x /\ y)) = show x <> ";" <> show y
    show (Dragging (startX /\ startY) ( curX /\ curY ) maybeItem) =
        "drag from " <> show startX <> ";" <> show startY <>
            " at " <> show curX <> ";" <> show curY <>
            " : " <> maybe "?" show maybeItem
    show (DropAt ( curX /\ curY ) maybeItem) =
        "drop at " <> show curX <> ";" <> show curY <>
            " : " <> maybe "?" show maybeItem