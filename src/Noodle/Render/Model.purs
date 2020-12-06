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
import Noodle.Render.Component.Patch.Model as Patch
import Noodle.Render.Component.Patch.Layout as Layout
import Noodle.Render.Context (Context)
import Noodle.Render.Context (init) as Context


type MousePos = Position


type Model d c n =
    -- TODO: use UUID to store inlets?
    { context :: Context d c n
    , debug :: Maybe (DebugBox.Model d c n)
    , patches :: UUID.ToPatch /-> Patch.Model d c n
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
    { context : Context.init
    , debug : Nothing
    --, debug : Just DebugBox.init
    --, offsets : UUID.ToPatch /-> Position
    , patches : Map.empty
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
