module Rpd.Network
    ( Network(..)
    , Patch(..)
    , Node(..)
    , Inlet(..)
    , Outlet(..)
    , Link(..)
    , InletFlow(..), OutletFlow(..)
    -- FIXME: do not expose constructors, provide all the optics as getters
    , empty
    ) where

import Prelude (class Eq, (==), (&&), class Show, show, (<>))

import Data.List as List
import Data.List (List)
import Data.Map as Map
import Data.Set (Set)
import Data.Tuple.Nested (type (/\))

import Rpd.Def
import Rpd.Path
import Rpd.Util (type (/->), Canceler, Flow, PushableFlow, PushF)


data InletFlow d = InletFlow (PushableFlow d)
data OutletFlow d = OutletFlow (PushableFlow d)


--data ProcessFlow d = ProcessFlow (PushableFlow (Int /\ d))


data Network d =
    Network
        { name :: String
        , patchDefs :: List (PatchDef d)
        }
        { patches :: PatchId /-> Patch d
        , nodes :: NodePath /-> Node d
        , inlets :: InletPath /-> Inlet d
        , outlets :: OutletPath /-> Outlet d
        , links :: LinkId /-> Link
        , cancelers ::
            { links :: LinkId /-> Canceler
            , nodes :: NodePath /-> Canceler
            , inlets :: InletPath /-> Canceler
            }
        }
data Patch d =
    Patch
        PatchId
        (PatchDef d)
        { nodes :: Set NodePath
        }
data Node d =
    Node
        NodePath -- (NodeDef d)
        (NodeDef d)
        { inlets :: Set InletPath
        , outlets :: Set OutletPath
        , processFlow :: PushableFlow (Int /\ d) -- TODO: can it be just Flow?
        --, inletsFlow :: InletsFlow d
        --, outletsFlow :: OutletsFlow d
        }
data Inlet d =
    Inlet
        InletPath
        (InletDef d)
        { flow :: InletFlow d
        -- flow :: PushableFlow d
        -- sources :: Set (DataSource d)
        }
data Outlet d =
    Outlet
        OutletPath
        (OutletDef d)
        { flow :: OutletFlow d
        -- flow :: PushableFlow d
        }
data Link = Link OutletPath InletPath


empty :: forall d. String -> Network d
empty name =
    Network
        { name
        , patchDefs : List.Nil
        }
        { patches : Map.empty
        , nodes : Map.empty
        , inlets : Map.empty
        , outlets : Map.empty
        , links : Map.empty
        , cancelers :
            { links : Map.empty
            , inlets : Map.empty
            , nodes : Map.empty
            }
        }


instance eqPatch :: Eq (Patch d) where
    eq (Patch idA _ _) (Patch idB _ _) = (idA == idB)

instance eqNode :: Eq (Node d) where
    eq (Node pathA _ _) (Node pathB _ _) = (pathA == pathB)

instance eqInlet :: Eq (Inlet d) where
    eq (Inlet pathA _ _) (Inlet pathB _ _) = (pathA == pathB)

instance eqOutlet :: Eq (Outlet d) where
    eq (Outlet pathA _ _) (Outlet pathB _ _) = (pathA == pathB)

instance eqLink :: Eq Link where
    eq (Link outletA inletA) (Link outletB inletB) = (outletA == outletB) && (inletA == inletB)


instance showLink :: Show Link where
    show (Link outletPath inletPath) = "Link " <> show outletPath <> " -> " <> show inletPath
