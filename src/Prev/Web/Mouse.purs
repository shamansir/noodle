module Prev.Web.Mouse where


import Prelude

import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.Tuple.Nested ((/\), type (/\))

import Web.UIEvent.MouseEvent as ME
import Data.Vec2 (Pos, (<+>))



type State a = State' a a a


data State' focusable clickable draggable
    = Move Pos (Maybe focusable)
    | Click Pos clickable
    | StartDrag Pos draggable
    | Dragging Pos Pos draggable
    | DropAt Pos draggable


init :: forall f c d. State' f c d
init = Move (0.0 <+> 0.0) Nothing


updatePos :: forall f c d. (Pos -> Pos) -> State' f c d -> State' f c d
updatePos f (Move pos v) = Move (f pos) v
updatePos f (Click pos v) = Click (f pos) v
updatePos f (StartDrag pos v) = StartDrag (f pos) v
updatePos f (Dragging start pos v) = Dragging start (f pos) v
updatePos f (DropAt pos v) = DropAt (f pos) v


apply
    :: forall a
     . (Pos -> Maybe a)
    -> ME.MouseEvent
    -> State a
    -> State a
apply f =
    apply' f (const Just) (const Just) (const Just) (const Just)


apply'
    :: forall focusable clickable draggable
     . (Pos -> Maybe focusable)
    -> (Pos -> focusable -> Maybe clickable)
    -> (Pos -> clickable -> Maybe draggable)
    -> (Pos -> clickable -> Maybe focusable)
    -> (Pos -> draggable -> Maybe focusable)
    -> ME.MouseEvent
    -> State' focusable clickable draggable
    -> State' focusable clickable draggable
apply' pToF fToC cToD cToF dToF event curState =
    analyze curState
    where
        buttonDown = ME.buttons event == 1
        nextPos = (toNumber $ ME.clientX event) <+> (toNumber $ ME.clientY event)
        analyze (Move _ _) =
            case buttonDown /\ (pToF nextPos >>= fToC nextPos) of
                true /\ Just clickable ->
                    Click nextPos clickable
                _ /\ _ -> Move nextPos $ pToF nextPos
        analyze (Click clickPos clickable) =
            case buttonDown /\ cToD nextPos clickable of
                true /\ Just draggable ->
                    StartDrag clickPos draggable
                _ /\ _ -> Move nextPos $ cToF nextPos clickable
        analyze (StartDrag clickPos draggable) =
            if buttonDown then
                Dragging clickPos nextPos draggable
            else Move nextPos $ dToF nextPos draggable
        analyze (Dragging clickPos _ draggable) =
            if buttonDown then
                Dragging clickPos nextPos draggable
            else DropAt nextPos draggable
        analyze (DropAt _ draggable) =
            {- if buttonDown then
                DropAt nextPos item
            else -}
            -- Move nextPos $ Just item
            Move nextPos $ dToF nextPos draggable


instance showMouse :: (Show f, Show c, Show d) => Show (State' f c d) where
    show (Move curPos maybeItem) = show curPos <> " : " <> show maybeItem
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
