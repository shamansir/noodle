module App.Mouse where


import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Int (toNumber)
import Data.Tuple.Nested ((/\), type (/\))
import Halogen.Svg.Attributes (a)
import Web.UIEvent.MouseEvent as ME


data State a
    = Move ( Number /\ Number )
    | Click ( Number /\ Number ) a
    | StartDrag ( Number /\ Number ) a
    | Dragging ( Number /\ Number ) ( Number /\ Number ) a
    | DropAt ( Number /\ Number ) a


init :: forall a. State a
init = Move ( 0.0 /\ 0.0 )


apply :: forall a. ((Number /\ Number) -> Maybe a) -> ME.MouseEvent -> State a -> State a
apply toItem event curState =
    analyze curState
    where
        buttonDown = ME.buttons event == 1
        nextPos = (toNumber $ ME.clientX event) /\ (toNumber $ ME.clientY event)
        analyze (Move _) =
            case buttonDown /\ (toItem nextPos) of
                true /\ Just item -> Click nextPos item
                _ -> Move nextPos
        analyze (Click clickPos item) =
            if buttonDown then
                StartDrag clickPos item
            else Move nextPos
        analyze (StartDrag clickPos item) =
            if buttonDown then
                Dragging clickPos nextPos item
            else Move nextPos
        analyze (Dragging clickPos _ item) =
            if buttonDown then
                Dragging clickPos nextPos item
            else DropAt nextPos item
        analyze (DropAt _ _) =
            {- if buttonDown then
                DropAt nextPos item
            else -} Move nextPos


instance showMouse :: Show a => Show (State a) where
    show (Move (x /\ y)) = show x <> ";" <> show y
    show (Click ( curX /\ curY ) item) =
        "click " <> show curX <> ";" <> show curY <>
            " : " <> show item
    show (StartDrag ( curX /\ curY ) item) =
        "start drag " <> show curX <> ";" <> show curY <>
            " : " <> show item
    show (Dragging (startX /\ startY) ( curX /\ curY ) item) =
        "drag from " <> show startX <> ";" <> show startY <>
            " to " <> show curX <> ";" <> show curY <>
            " : " <> show item
    show (DropAt ( curX /\ curY ) item) =
        "drop at " <> show curX <> ";" <> show curY <>
            " : " <> show item


mapPos :: forall a. ((Number /\ Number) -> (Number /\ Number)) -> State a -> State a
mapPos f (Move pos) = Move $ f pos
mapPos f (Click pos i) = Click (f pos) i
mapPos f (StartDrag pos i) = Click (f pos) i
mapPos f (Dragging start pos i) = Dragging (f start) (f pos) i
mapPos f (DropAt pos i) = DropAt (f pos) i


shift' :: (Number /\ Number) -> (Number /\ Number) -> (Number /\ Number)
shift' (offX /\ offY) (x /\ y) = (x - offX) /\ (y - offY)


shift :: forall a. (Number /\ Number) -> State a -> State a
shift offset = mapPos $ shift' offset