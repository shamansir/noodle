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
import Noodle.Render.Layout (Layout)
import Noodle.Render.DebugBox as DebugBox
import Noodle.Render.Component.Patch.Layout as Layout


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
    , positions :: UUID.Tagged /-> Position
    -- , positions ::
    --     { inlets :: UUID.ToInlet /-> Position
    --     , outlets :: UUID.ToOutlet /-> Position
    --     }
    , layout :: Layout d n
    }


{-
data Perform d c n
    = UpdatePositions
    | TryConnecting (R.Outlet d c) (R.Inlet d c)
    | TryToPinNode (R.Node d n) Position
    | TryRemovingNode (R.Node d n)
    | StopPropagation Event
-}


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
    , positions : Map.empty -- FIXME: exclude nodes (they are stored in the `packing`)
    , layout :
            Layout.loadIntoStacks Layout.defaultLayerSize Layout.getNodeSize nw
                # Layout.initWithStacks
    }


updatePositions
    :: forall d c n x
     . ((UUID.Tagged /-> Position) -> x)
    -> R.Network d c n
    -> Effect x
updatePositions ack (R.Network nw) = do
    positions <- collectPositions $ loadUUIDs $ Map.keys nw.registry
    pure $ ack $ convertPositions positions


loadUUIDs :: Set UUID.Tagged -> Array { uuid :: String, kind :: String }
loadUUIDs uuids = UUID.encode <$> Set.toUnfoldable uuids


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
