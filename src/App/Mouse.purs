module App.Mouse where


import Prelude
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.Tuple.Nested ((/\), type (/\))
import Web.UIEvent.MouseEvent as ME
import Data.Vec2 (Pos, (<+>))


data State a
    = Move Pos
    | Click Pos a
    | StartDrag Pos a
    | Dragging Pos Pos a
    | DropAt Pos a


init :: forall a. State a
init = Move $ 0.0 <+> 0.0


apply :: forall a. (Pos -> Maybe a) -> ME.MouseEvent -> State a -> State a
apply toItem event curState =
    analyze curState
    where
        buttonDown = ME.buttons event == 1
        nextPos = (toNumber $ ME.clientX event) <+> (toNumber $ ME.clientY event)
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
