module Noodle.Render.Action where

import Prelude (Unit)

import Effect (Effect)

import Web.UIEvent.MouseEvent as ME

import Noodle.API.Action (Action) as Core
import Noodle.Util (Position, type (/->))
import Noodle.UUID (UUID)
import Noodle.UUID as UUID

import Noodle.Network as R
import Noodle.Render.Model (MousePos)
import Noodle.Render.Renderer (Routed(..))
import Noodle.Render.DebugBox (Action) as DebugBox


type PushF d c n = Action d c n -> Effect Unit


type RoutedAction d c n = Routed (Action d c n) (Core.Action d c n)


data Action d c n
    = NoOp
    | EnableDebug
    | DisableDebug
    | ClickBackground ME.MouseEvent
    | MouseMove MousePos
    | MouseUp MousePos
    | ClickInlet (R.Inlet d c) ME.MouseEvent
    | ClickOutlet (R.Outlet d c) ME.MouseEvent
    | ClickNodeTitle (R.Node d n) ME.MouseEvent
    | ClickRemoveButton (R.Node d n) ME.MouseEvent
    | ClickLink R.Link ME.MouseEvent
    | PinNode (R.Node d n) Position
    | StorePositions (UUID.Tagged /-> Position)
    | ToDebugBox DebugBox.Action


core :: forall d c n. Core.Action d c n -> RoutedAction d c n
core = FromCore


my :: forall d c n. Action d c n -> RoutedAction d c n
my = FromUI


