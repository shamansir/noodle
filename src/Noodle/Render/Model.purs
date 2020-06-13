module Noodle.Render.Model where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map as Map
import Data.Vec2 (Vec2(..))
import Data.Vec2 as Vec2
import Data.Set as Set
import Data.Set (Set)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (catMaybes) as Array

import Noodle.Util (type (/->), Rect, Position)

import Noodle.Network as R
import Noodle.UUID (UUID)
import Noodle.UUID as UUID
import Noodle.Path as P

import Noodle.Render.Layout as Layout
import Noodle.Render.Layout (Layout, ZIndex(..))
import Noodle.Render.DebugBox as DebugBox


type MousePos = Position


type Model d c n =
    -- TODO: use UUID to store inlets?
    { lastInletData :: P.ToInlet /-> d
    , lastOutletData :: P.ToOutlet /-> d
    , debug :: Maybe (DebugBox.Model d c n)
    -- , uuidToChannelDef :: UUID /-> T.ChannelDefAlias
    -- , uuidToNodeDef :: UUID /-> T.NodeDefAlias
    , uuidToChannel :: UUID /-> c
    , mousePos :: MousePos
    , dragging :: DragState d c n
    , positions :: UUID.Tagged /-> Position
    -- , positions ::
    --     { inlets :: UUID.ToInlet /-> Position
    --     , outlets :: UUID.ToOutlet /-> Position
    --     }
    , layout :: Layout d n
    }


-- data ZIndex = ZIndex Int


data Emplacement
    = NotDetermined
    | Pinned ZIndex Position
    | Packed Position Rect


type LinkEnds = { from :: Position, to :: Position }
type LinkTransform = { from :: Position, angle :: Number, length :: Number }


{-
data Perform d c n
    = UpdatePositions
    | TryConnecting (R.Outlet d c) (R.Inlet d c)
    | TryToPinNode (R.Node d n) Position
    | TryRemovingNode (R.Node d n)
    | StopPropagation Event
-}


data DragSubject d c n
    = DragNode (R.Node d n)
    | DragLink (R.Outlet d c)


data DragState d c n
    = NotDragging
    | Dragging (DragSubject d c n)


instance showDragState :: Show (DragState d c n) where
    show NotDragging = "not dragging"
    show (Dragging (DragNode (R.Node _ nPath _ _ _))) = "dragging node " <> show nPath
    show (Dragging (DragLink (R.Outlet _ oPath _ _))) = "dragging link from " <> show oPath


init :: forall d c n. R.Network d c n -> Model d c n
init nw =
    { lastInletData : Map.empty
    , lastOutletData : Map.empty
    , debug : Nothing
    --, debug : Just DebugBox.init
    -- , uuidToChannelDef : Map.empty
    -- , uuidToNodeDef : Map.empty
    , uuidToChannel : Map.empty
    , mousePos : { x : -1.0, y : -1.0 }
    , dragging : NotDragging
    , positions : Map.empty -- FIXME: exclude nodes (they are stored in the `packing`)
    , layout :
            Layout.loadIntoStacks defaultLayerSize getNodeSize nw
                # Layout.initWithStacks
    }


defaultLayerSize :: Layout.LayerSize
defaultLayerSize = Layout.LayerSize { width : 1000.0, height : 1000.0 }


dragZIndex :: ZIndex
dragZIndex = ZIndex 1000


getLinkTransform :: { from :: Position, to :: Position } -> LinkTransform
getLinkTransform { from, to } =
    let { angle, length } = Vec2.arrow (Vec2 from.x from.y /\ Vec2 to.x to.y)
    in { from, angle, length }


updatePositions
    :: forall d c n x
     . ((UUID.Tagged /-> Position) -> x)
    -> R.Network d c n
    -> Effect x
updatePositions ack (R.Network nw) = do
    positions <- collectPositions $ loadUUIDs $ Map.keys nw.registry
    pure $ ack $ convertPositions positions


-- FIXME: move to the Toolkit renderer
getNodeSize :: forall d n. R.Node d n -> Layout.NodeSize
getNodeSize _ = Layout.NodeSize { width : 100.0, height : 70.0 }


loadUUIDs :: Set UUID.Tagged -> Array { uuid :: String, kind :: String }
loadUUIDs uuids = UUID.encode <$> Set.toUnfoldable uuids


toLocalPos :: (UUID.Tagged /-> Position) -> UUID.ToPatch -> Position -> Position
toLocalPos positions patchUuid pos =
    let
       patchPos = Map.lookup (UUID.liftTagged patchUuid) positions
                        # fromMaybe { x : 0.0, y : 0.0 }
    in
        { x : pos.x - patchPos.x
        -- FIXME: 25.0 is a height of inlets area
        , y : pos.y - 25.0 - patchPos.y
        }


-- TODO: move to Renderer.Layout?
convertPositions
    :: Array
            { uuid :: String
            , kind :: String
            , pos :: Position
            }
    -> UUID.Tagged /-> Position
convertPositions srcPositions =
    Map.fromFoldable $ Array.catMaybes $ decodePos <$> srcPositions
    where
        decodePos
            ::
                { uuid :: String
                , kind :: String
                , pos :: Position
                }
            -> Maybe (UUID.Tagged /\ Position)
        decodePos { kind, uuid, pos } =
            (\tagged -> tagged /\ pos) <$> UUID.decode { uuid, kind }


collectPositions
    :: Array { uuid :: String, kind :: String }
    -> Effect
            (Array
                { uuid :: String
                , kind :: String
                , pos :: Position
                }
            )
collectPositions = const $ pure []
