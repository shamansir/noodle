module Noodle.Render.Component.Patch.Model where

import Prelude

import Data.Map (lookup) as Map
import Data.Maybe (fromMaybe)

import Noodle.Network (Node, Outlet, Patch) as R
import Noodle.Util (type (/->), Position)
import Noodle.UUID as UUID


type Positions = (UUID.Tagged /-> Position)


type Model d c n =
    { patch :: R.Patch d c n
    , dragging :: DragState d c n
    , positions :: Positions
    , layout :: Layout d n
    }


data DragSubject d c n
    = DragNode (R.Node d n)
    | DragLink (R.Outlet d c)


data DragState d c n
    = NotDragging
    | Dragging (DragSubject d c n)


toLocalPos :: Positions -> UUID.ToPatch -> Position -> Position
toLocalPos positions patchUuid pos =
    let
       patchPos = Map.lookup (UUID.liftTagged patchUuid) positions
                        # fromMaybe { x : 0.0, y : 0.0 }
    in
        { x : pos.x - patchPos.x
        -- FIXME: 25.0 is a height of inlets area
        , y : pos.y - 25.0 - patchPos.y
        }
