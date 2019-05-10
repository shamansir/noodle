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


import Prelude (class Eq, (==), (&&), class Show, show, (<>))

import Data.Maybe (Maybe)
import Data.List as List
import Data.List (List)
import Data.Map as Map
import Data.Set (Set)
import Data.Set (empty) as Set
import Data.Tuple.Nested ((/\), type (/\))

import Rpd.Def
import Rpd.Path
import Rpd.UUID (UUID)
import Rpd.UUID (ToPatch(..), ToNode(..), ToInlet(..), ToOutlet(..), ToLink(..)) as UUID
import Rpd.Process (InletInNode, OutletInNode)
import Rpd.Util (type (/->), Canceler, Flow, PushableFlow, PushF)


data InletFlow d = InletFlow (Flow d)
data InletsFlow d = InletsFlow (Flow (InletInNode /\ d))
data PushToInlet d = PushToInlet (PushF d)
data PushToInlets d = PushToInlets (PushF (InletInNode /\ d))
data OutletFlow d = OutletFlow (Flow d)
data OutletsFlow d = OutletsFlow (Flow (Maybe (OutletInNode /\ d)))
        -- FIXME: Flow (Maybe OutletInNode /\ d)
data PushToOutlet d = PushToOutlet (PushF d)
data PushToOutlets d = PushToOutlets (PushF (Maybe (OutletInNode /\ d)))
        -- FIXME: PushF (Maybe OutletInNode /\ d)


data Entity d
    = PatchEntity (Patch d)
    | NodeEntity (Node d)
    | InletEntity (Inlet d)
    | OutletEntity (Outlet d)
    | LinkEntity Link


data Network d =
    Network
        { name :: String
        , patchDefs :: List (PatchDef d)
        }
        { patches :: Set UUID.ToPatch
        , registry :: UUID /-> Entity d
        , pathToId :: Path /-> List UUID
        , cancelers ::
            -- UUID /-> Array Canceler
            { nodes :: UUID.ToNode /-> Array Canceler
            , inlets :: UUID.ToInlet /-> Array Canceler
            , outlets :: UUID.ToOutlet /-> Array Canceler
            , links :: UUID.ToLink /-> Array Canceler
            }
        }
data Patch d =
    Patch
        UUID
        PatchPath
        (PatchDef d)
        { nodes :: Set UUID.ToNode
        }
data Node d =
    Node
        UUID
        NodePath
        (NodeDef d)
        { inlets :: Set UUID.ToInlet
        , outlets :: Set UUID.ToOutlet
        , inletsFlow :: InletsFlow d
        , outletsFlow :: OutletsFlow d
        , pushToInlets :: PushToInlets d
        , pushToOutlets :: PushToOutlets d
        }
data Inlet d =
    Inlet
        UUID
        InletPath
        (InletDef d)
        { flow :: InletFlow d
        , push :: PushToInlet d
        }
data Outlet d =
    Outlet
        UUID
        OutletPath
        (OutletDef d)
        { flow :: OutletFlow d
        , push :: PushToOutlet d
        }
data Link = Link UUID (UUID.ToOutlet /\ UUID.ToInlet)


empty :: forall d. String -> Network d
empty name =
    Network
        { name
        , patchDefs : List.Nil
        }
        { patches : Set.empty
        , registry : Map.empty
        , pathToId : Map.empty
        , cancelers :
            { nodes : Map.empty
            , inlets : Map.empty
            , outlets : Map.empty
            , links : Map.empty
            }
        }


instance eqPatch :: Eq (Patch d) where
    eq (Patch idA _ _ _) (Patch idB _ _ _) = (idA == idB)

instance eqNode :: Eq (Node d) where
    eq (Node pathA _ _ _) (Node pathB _ _ _) = (pathA == pathB)

instance eqInlet :: Eq (Inlet d) where
    eq (Inlet pathA _ _ _) (Inlet pathB _ _ _) = (pathA == pathB)

instance eqOutlet :: Eq (Outlet d) where
    eq (Outlet pathA _ _ _) (Outlet pathB _ _ _) = (pathA == pathB)

instance eqLink :: Eq Link where
    eq
        (Link _ (UUID.ToOutlet outletA /\ UUID.ToInlet inletA))
        (Link _ (UUID.ToOutlet outletB /\ UUID.ToInlet inletB))
        = (outletA == outletB) && (inletA == inletB)

instance showLink :: Show Link where
    show (Link outletPath inletPath) = "Link " <> show outletPath <> " -> " <> show inletPath
