module Tmp where

import Data.Map (Map, insert)
import Data.Map as Map
import Data.Maybe (Maybe)
import Signal as S
import Data.Function (apply, applyFlipped)

-- RPD

type Id = String

type NetworkId = Id
type PatchId = Id
type NodeId = Id
type ChannelId = Id
type InletId = ChannelId
type OutletId = ChannelId
type LinkId = Id

-- `n` — node type
-- `c` — channel type
-- `a` — data type
-- `x` — error type

data NetworkMsg n c
    = AddPatch PatchId String
    | AddPatch' PatchId
    | RemovePatch PatchId
    | SelectPatch PatchId
    | DeselectPatch
    | EnterPatch PatchId
    | ExitPatch PatchId
    | AddNode PatchId n NodeId String
    | AddNode' PatchId n NodeId
    | AddInlet PatchId NodeId c InletId String
    | AddInlet' PatchId NodeId c InletId
    | AddOutlet PatchId NodeId c OutletId String
    | AddOutlet' PatchId NodeId c OutletId
    | Connect PatchId NodeId NodeId InletId OutletId
    | Disconnect PatchId NodeId NodeId InletId OutletId
    -- Hide Inlet'
    -- Disable Link'

data FlowMsg c a x
    = Send (Inlet' c) a -- send data to Outlets?
    | Attach (Inlet' c) (S.Signal a) -- send streams to Outlets?
    | SendError (Inlet' c) x

type Network' n c a x =
    { id :: NetworkId
    , patches :: Map PatchId (Patch n c a x)
    , selected :: Maybe PatchId
    , entered :: Array PatchId
    }

type Patch' n c a x =
    { id :: PatchId
    , title :: String
    , nodes :: Map NodeId (Node n c a x)
    , links :: Map LinkId (Link c a x)
    }

type Node' n c a x =
    { id :: NodeId
    , title :: String
    , type :: n
    , inlets :: Map InletId (Inlet c a x)
    , outlets :: Map OutletId (Outlet c a x)
    }

type Inlet' c =
    { id :: InletId
    , label :: String
    , type :: c
    }

type Outlet' c =
    { id :: OutletId
    , label :: String
    , type :: c
    }

-- type Link' c =
--     { id :: LinkId
--     , inlet :: Inlet' c
--     , outlet :: Outlet' c
--     }

data Flow a x
    = Bang
    | Data a
    | Error x

type FSignal a x = S.Signal (Flow a x)

data Network n c a x = Network (Network' n c a x) (FSignal a x)

data Patch n c a x = Patch (Patch' n c a x) (FSignal a x)

data Node n c a x = Node (Node' n c a x) (FSignal a x)

data Inlet c a x = Inlet (Inlet' c) (FSignal a x)

data Outlet c a x = Outlet (Outlet' c) (FSignal a x)

data Link c a x = Link (Outlet c a x) (Inlet c a x)

-- Elm-style operators

infixr 0 apply as <|
infixl 1 applyFlipped as |>

createPatch' :: forall n c a x. String -> Patch n c a x
createPatch' title =
    Patch
        { id : "test"
        , title : title
        , nodes : Map.empty
        , links : Map.empty
        }
        (S.constant Bang)

createNode' :: forall n c a x. String -> n -> Node n c a x
createNode' title nodeType =
    Node
        { id : "test"
        , title : title
        , type : nodeType
        , inlets : Map.empty
        , outlets : Map.empty
        }
        (S.constant Bang)

createInlet' :: forall c a x. String -> c -> Inlet c a x
createInlet' label inletType =
    Inlet
        { id : "test"
        , label : label
        , type : inletType
        }
        (S.constant Bang)

createOutlet' :: forall c a x. String -> c -> Outlet c a x
createOutlet' label outletType =
    Outlet
        { id : "test"
        , label : label
        , type : outletType
        }
        (S.constant Bang)

connect' :: forall c a x. Outlet c a x -> Inlet c a x -> Link c a x
connect' outlet inlet =
    Link outlet inlet

addNode' :: forall n c a x. Node n c a x -> Patch n c a x -> Patch n c a x
addNode' node@(Node node' nodeSignal) (Patch patch' patchSignal) =
    Patch
        (patch' { nodes = patch'.nodes |> insert node'.id node })
        (S.merge patchSignal nodeSignal)

addInlet' :: forall n c a x. Inlet c a x -> Node n c a x -> Node n c a x
-- addInlet inlet'@(Inlet inlet inletSignal) (Node node nodeSignal) =
--  Node (node { inlets = inlet' : node.inlets }) (S.merge nodeSignal inletSignal)
addInlet' inlet@(Inlet inlet' inletSignal) (Node node' nodeSignal) =
    Node
        (node' { inlets = node'.inlets |> insert inlet'.id inlet })
        (S.merge nodeSignal inletSignal)

addOutlet' :: forall n c a x. Outlet c a x -> Node n c a x -> Node n c a x
addOutlet' outlet@(Outlet outlet' outletSignal) (Node node' nodeSignal) =
    Node
        (node' { outlets = node'.outlets |> insert outlet'.id outlet })
        (S.merge nodeSignal outletSignal)

attach' :: forall c a x. S.Signal a -> Inlet c a x -> Inlet c a x
attach' dataSignal (Inlet inlet inletSignal) =
    let
        mappedSignal = (\d -> Data d) S.<~ dataSignal
    in
        Inlet inlet (S.merge inletSignal mappedSignal)

attachErrors' :: forall c a x. S.Signal x -> Inlet c a x -> Inlet c a x
attachErrors' errorSignal (Inlet inlet inletSignal) =
    let
        mappedSignal = (\x -> Error x) S.<~ errorSignal
    in
        Inlet inlet (S.merge inletSignal mappedSignal)

send' :: forall c a x. a -> Inlet c a x -> Inlet c a x
send' v =
    attach' (S.constant v)

sendError' :: forall c a x. x -> Inlet c a x -> Inlet c a x
sendError' e =
    attachErrors' (S.constant e)

-- instance showPercentage :: Show Percentage where
--   show (Percentage n) = show n <> "%"

-- rendering

