module Rpd.Network
    ( Network(..)
    , Patch(..)
    , Node(..)
    , Inlet(..)
    , Outlet(..)
    , Link(..)
    , Entity(..)
    , InletFlow(..), OutletFlow(..)
    , InletsFlow(..), OutletsFlow(..)
    , PushToInlet(..), PushToOutlet(..)
    , PushToInlets(..), PushToOutlets(..)
    -- FIXME: do not expose constructors, provide all the optics as getters
    , empty
    ) where


import Prelude (class Eq, (==))

import Data.Map as Map
import Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Maybe (Maybe)

import Data.Tuple.Nested ((/\), type (/\))

import Rpd.Path as Path
import Rpd.Path (Path)
import Rpd.UUID (UUID)
import Rpd.UUID as UUID
import Rpd.Util (type (/->), Canceler, Flow, PushF)
import Rpd.Process (ProcessF)


-- FIXME: UUID is internal and so should not be passed, I suppose.
--        I'll leave it here temporarily just for the debug purpose.
data InletFlow d = InletFlow (Flow d)
data InletsFlow d = InletsFlow (Flow (Path.ToInlet /\ UUID.ToInlet /\ d))
data PushToInlet d = PushToInlet (PushF d)
data PushToInlets d = PushToInlets (PushF (Path.ToInlet /\ UUID.ToInlet /\ d))
data OutletFlow d = OutletFlow (Flow d)
data OutletsFlow d = OutletsFlow (Flow (Path.ToOutlet /\ UUID.ToOutlet /\ d))
        -- FIXME: Flow (Maybe OutletInNode /\ d)
data PushToOutlet d = PushToOutlet (PushF d)
data PushToOutlets d = PushToOutlets (PushF (Path.ToOutlet /\ UUID.ToOutlet /\ d))
        -- FIXME: PushF (Maybe OutletInNode /\ d)


-- TODO: Make `Entity` a type kind?
data Entity d
    = PatchEntity (Patch d)
    | NodeEntity (Node d)
    | InletEntity (Inlet d)
    | OutletEntity (Outlet d)
    | LinkEntity Link


data Network d =
    Network
        { name :: String
        , patches :: Seq UUID.ToPatch
        , registry :: UUID.Tagged /-> Entity d
        -- , pathToId :: Path /-> Set UUID
        , pathToId :: Path /-> UUID.Tagged
        , cancelers :: UUID /-> Array Canceler
            -- { nodes :: UUID.ToNode /-> Array Canceler
            -- , inlets :: UUID.ToInlet /-> Array Canceler
            -- , outlets :: UUID.ToOutlet /-> Array Canceler
            -- , links :: UUID.ToLink /-> Array Canceler
            -- }
        }
data Patch d =
    Patch
        UUID.ToPatch
        Path.ToPatch
        { nodes :: Seq UUID.ToNode
        , links :: Seq UUID.ToLink
        }
data Node d =
    Node
        UUID.ToNode
        Path.ToNode
        (ProcessF d)
        { inlets :: Seq UUID.ToInlet
        , outlets :: Seq UUID.ToOutlet
        , inletsFlow :: InletsFlow d
        , outletsFlow :: OutletsFlow d
        , pushToInlets :: PushToInlets d
        , pushToOutlets :: PushToOutlets d
        }
data Inlet d =
    Inlet
        UUID.ToInlet
        Path.ToInlet
        { flow :: InletFlow d
        , push :: PushToInlet d
        }
data Outlet d =
    Outlet
        UUID.ToOutlet
        Path.ToOutlet
        { flow :: OutletFlow d
        , push :: PushToOutlet d
        }
data Link =
    Link
        UUID.ToLink
        { outlet :: UUID.ToOutlet
        , inlet :: UUID.ToInlet
        }


empty :: forall d. String -> Network d
empty networkName =
    Network
        { name : networkName
        , patches : Seq.empty
        , registry : Map.empty
        , pathToId : Map.empty
        , cancelers : Map.empty
            -- { nodes : Map.empty
            -- , inlets : Map.empty
            -- , outlets : Map.empty
            -- , links : Map.empty
            -- }
        }


instance eqPatch :: Eq (Patch d) where
    eq (Patch pidA _ _) (Patch pidB _ _) = (pidA == pidB)

instance eqNode :: Eq (Node d) where
    eq (Node nidA _ _ _) (Node nidB _ _ _) = (nidA == nidB)

instance eqInlet :: Eq (Inlet d) where
    eq (Inlet iidA _ _) (Inlet iidB _ _) = (iidA == iidB)

instance eqOutlet :: Eq (Outlet d) where
    eq (Outlet oidA _ _) (Outlet oidB _ _) = (oidA == oidB)

instance eqLink :: Eq Link where
    eq (Link lidA _) (Link lidB _) = (lidA == lidB)
