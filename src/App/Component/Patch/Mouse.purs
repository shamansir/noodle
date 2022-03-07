module App.Component.Patch.Mouse where


import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec2 (Pos)
import Data.Maybe (Maybe(..))

import Noodle.Node as Node
import Noodle.Patch (Patch)
import Noodle.Patch as Patch

import App.Mouse as Mouse

import Web.UIEvent.MouseEvent as ME


type Focusable = Clickable


data Clickable
    = Header Node.Id
    | Inlet (Node.Id /\ Node.InletId)
    | Outlet (Node.Id /\ Node.OutletId)


data Draggable
    = Node Node.Id
    | Link (Node.Id /\ Node.OutletId) (Maybe (Node.Id /\ Node.InletId))


type State = Mouse.State' (Pos /\ Focusable) (Pos /\ Clickable) (Pos /\ Draggable)


-- apply = Mouse.apply' Just


init :: State
init = Mouse.init



apply
    :: (Pos -> Maybe (Pos /\ Focusable))
    -> (Pos -> Clickable -> Maybe Draggable)
    -> (Pos -> Draggable -> Maybe Focusable)
    -> ME.MouseEvent
    -> State
    -> State
apply pToF cToD dToF =
    Mouse.apply'
        pToF
        (const Just)
        (\nextPos (origPos /\ clickable) -> (/\) origPos <$> cToD nextPos clickable)
        (const Just)
        (\nextPos (origPos /\ focusable) -> (/\) origPos <$> dToF nextPos focusable)


{- clickableToDraggable :: Clickable -> Maybe Draggable
clickableToDraggable (Header nodeId) = Just $ Node nodeId
clickableToDraggable (Inlet inletPath) = Nothing -- FIXME: could be link
clickableToDraggable (Outlet outletPath) = Just $ LinkFrom outletPath -}


instance showClickable :: Show Clickable where
    show (Header n) = "header " <> n
    show (Inlet path) = "inlet " <> show path
    show (Outlet path) = "outlet " <> show path


instance showDraggable :: Show Draggable where
    show (Node n) = "node " <> n
    show (Link outlet maybeInlet) = "link " <> show outlet <> " - " <> show maybeInlet