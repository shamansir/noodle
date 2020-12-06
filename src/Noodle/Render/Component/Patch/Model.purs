module Noodle.Render.Component.Patch.Model where

import Prelude

import Data.Map (lookup) as Map
import Data.Maybe (fromMaybe)
import Data.Vec2 (Vec2(..))
import Data.Vec2 as Vec2
import Data.Tuple.Nested ((/\))

import Noodle.Network (Node(..), Outlet(..), Patch, Network) as R
import Noodle.Util (type (/->), Rect, Position)
import Noodle.UUID as UUID

import Noodle.Render.Component.Patch.Layout (Layout, ZIndex(..))
import Noodle.Render.Component.Patch.Layout as Layout


type Positions = (UUID.Tagged /-> Position)


type Model d c n =
    { patch :: R.Patch d c n
    , dragging :: DragState d c n
    , layout :: Layout d n
    }


data DragSubject d c n
    = DragNode (R.Node d n)
    | DragLink (R.Outlet d c)


data DragState d c n
    = NotDragging
    | Dragging (DragSubject d c n)


data Emplacement
    = NotDetermined
    | Pinned ZIndex Position
    | Packed Position Rect


type LinkEnds = { from :: Position, to :: Position }
type LinkTransform = { from :: Position, angle :: Number, length :: Number }


-- TODO: use lens to get nodes from the patch
-- FIXME: do not require network to be passed
init :: forall d c n. R.Patch d c n -> Model d c n
init patch =
    { patch : patch
    , dragging : NotDragging
    , layout : Layout.loadIntoStack Layout.defaultLayerSize Layout.getNodeSize patch
    }


dragZIndex :: ZIndex
dragZIndex = ZIndex 1000


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


getLinkTransform :: { from :: Position, to :: Position } -> LinkTransform
getLinkTransform { from, to } =
    let { angle, length } = Vec2.arrow (Vec2 from.x from.y /\ Vec2 to.x to.y)
    in { from, angle, length }


instance showDragState :: Show (DragState d c n) where
    show NotDragging = "not dragging"
    show (Dragging (DragNode (R.Node _ nPath _ _ _ _))) = "dragging node " <> show nPath
    show (Dragging (DragLink (R.Outlet _ oPath _ _))) = "dragging link from " <> show oPath
