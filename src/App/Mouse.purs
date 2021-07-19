module App.Mouse where


import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Int (toNumber)
import Data.Tuple.Nested ((/\), type (/\))
import Web.UIEvent.MouseEvent as ME
import Data.Vec2 (Vec2)
import Data.Vec2 as Vec2


data State a
    = Move Vec2
    | Click Vec2 a
    | StartDrag Vec2 a
    | Dragging Vec2 Vec2 a
    | DropAt Vec2 a


init :: forall a. State a
init = Move $ Vec2.make 0.0 0.0


apply :: forall a. ((Number /\ Number) -> Maybe a) -> ME.MouseEvent -> State a -> State a
apply toItem event curState =
    analyze curState
    where
        buttonDown = ME.buttons event == 1
        nextPos' = (toNumber $ ME.clientX event) /\ (toNumber $ ME.clientY event)
        nextPos = Vec2.fromTuple nextPos'
        analyze (Move _) =
            case buttonDown /\ (toItem nextPos') of
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
    show (Move curPos) = show curPos
    show (Click curPos item) =
        "click " <> show curPos <> " : " <> show item
    show (StartDrag startPos item) =
        "start drag " <> show startPos <> " : " <> show item
    show (Dragging startPos curPos item) =
        "drag from " <> show startPos <>
            " to " <> show curPos <>
            " : " <> show item
    show (DropAt curPos item) =
        "drop at " <> show curPos <> " : " <> show item
