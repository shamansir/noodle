module Web.Components.PatchArea.Types where

import Prelude

import Data.Map (Map)

import Play (Layout) as Play

import Noodle.Id (NodeR, InletR, OutletR, LinkR, FamilyR) as Id

import Front.Shared.Bounds
import Web.Layouts (NodePart(..)) as Layout
import Web.Class.WebRenderer (class WebLocator, ConstantShift, class WebEditor, spawnWebEditor)


newtype NodeZIndex = ZIndex Int
derive newtype instance Eq NodeZIndex
derive newtype instance Ord NodeZIndex
instance Bounded NodeZIndex where
    top = ZIndex 1000
    bottom = ZIndex 0


type Locator = ConstantShift -- TODO: move to some root App config?


type LinkStart =
    { fromNode :: Id.NodeR
    , fromOutlet :: Id.OutletR
    }


type LinkEnd =
    { toNode :: Id.NodeR
    , toInlet :: Id.InletR
    }


data LockingTask
    = NoLock
    | DraggingNode Id.NodeR Delta
    | Connecting LinkStart PositionXY


type NodeGeometry =
    { bounds :: Bounds
    , z :: NodeZIndex
    , layout :: Play.Layout Layout.NodePart
    }


type NodesGeometry = Map Id.NodeR NodeGeometry


