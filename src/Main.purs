module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array ((:))
-- import Data.Monoid (mempty)
import Signal as S
import Signal.Time as ST

type Id = String

-- `n` — node type
-- `c` — channel type
-- `a` — data type
-- `x` — error type

-- type PatchModel n c a x =
--     { id :: Id
--     , title :: String
--     , nodes :: Array (Node n c a x)
--     , links :: Array (Link c a x)
--     }

data NetworkMsg n c a x
    = CreatePatch (Patch' n c a x)
    | SelectPatch (Patch' n c a x)
    | ClosePatch (Patch' n c a x)
    | AddNode (Node' n c)
    | AddInlet (Node' n c) (Inlet' c)
    | AddOutlet (Node' n c) (Outlet' c)
    | Connect (Inlet' c) (Outlet' c)
    | Disconnect (Inlet' c) (Outlet' c)
    -- Hide Inlet'
    -- Disable Link'

data FlowMsg c a x
    = Send (Inlet' c) -- send data to Outlets?
    | Attach (Inlet' c) (S.Signal a) -- send streams to Outlets?
    | SendError (Inlet' c)

type Patch' n c a x =
    { id :: Id
    , title :: String
    , nodes :: Array (Node' n c)
    , links :: Array (Link c a x)
    }

type Node' n c =
    { id :: Id
    , title :: String
    , type :: n
    , inlets :: Array (Inlet' c)
    , outlets :: Array (Outlet' c)
    }

type Inlet' c =
    { id :: Id
    , label :: String
    , type :: c
    }

type Outlet' c =
    { id :: Id
    , label :: String
    , type :: c
    }

data Flow a x = Bang | Data a | Error x

data Patch n c a x = Patch (Patch' n c a x) (S.Signal (Flow a x))

data Node n c a x = Node (Node' n c) (S.Signal (Flow a x))

data Inlet c a x = Inlet (Inlet' c) (S.Signal (Flow a x))

data Outlet c a x = Outlet (Outlet' c) (S.Signal (Flow a x))

data Link c a x = Link (Outlet c a x) (Inlet c a x)

createPatch :: forall n c a x. String -> Patch n c a x
createPatch title =
    Patch
        { id : "test"
        , title : title
        , nodes : []
        , links : []
        }
        (S.constant Bang)

createNode :: forall n c a x. String -> n -> Node n c a x
createNode title nodeType =
    Node
        { id : "test"
        , title : title
        , type : nodeType
        , inlets : []
        , outlets : []
        }
        (S.constant Bang)

createInlet :: forall c a x. String -> c -> Inlet c a x
createInlet label inletType =
    Inlet
        { id : "test"
        , label : label
        , type : inletType
        }
        (S.constant Bang)

createOutlet :: forall c a x. String -> c -> Outlet c a x
createOutlet label outletType =
    Outlet
        { id : "test"
        , label : label
        , type : outletType
        }
        (S.constant Bang)

connect :: forall c a x. Outlet c a x -> Inlet c a x -> Link c a x
connect outlet inlet =
    Link outlet inlet

addNode :: forall n c a x. Node n c a x -> Patch n c a x -> Patch n c a x
addNode (Node node nodeSignal) (Patch patch patchSignal) =
    Patch (patch { nodes = node : patch.nodes }) (S.merge patchSignal nodeSignal)

addInlet :: forall n c a x. Inlet c a x -> Node n c a x -> Node n c a x
-- addInlet inlet'@(Inlet inlet inletSignal) (Node node nodeSignal) =
--  Node (node { inlets = inlet' : node.inlets }) (S.merge nodeSignal inletSignal)
addInlet (Inlet inlet inletSignal) (Node node nodeSignal) =
    Node (node { inlets = inlet : node.inlets }) (S.merge nodeSignal inletSignal)

addOutlet :: forall n c a x. Outlet c a x -> Node n c a x -> Node n c a x
addOutlet (Outlet outlet outletSignal) (Node node nodeSignal) =
    Node (node { outlets = outlet : node.outlets }) (S.merge nodeSignal outletSignal)

attach :: forall c a x. S.Signal a -> Inlet c a x -> Inlet c a x
attach dataSignal (Inlet inlet inletSignal) =
    let
        mappedSignal = (\d -> Data d) S.<~ dataSignal
    in
        Inlet inlet (S.merge inletSignal mappedSignal)

attachErrors :: forall c a x. S.Signal x -> Inlet c a x -> Inlet c a x
attachErrors errorSignal (Inlet inlet inletSignal) =
    let
        mappedSignal = (\x -> Error x) S.<~ errorSignal
    in
        Inlet inlet (S.merge inletSignal mappedSignal)

-- instance showPercentage :: Show Percentage where
--   show (Percentage n) = show n <> "%"

-- one :: forall a. (Semiring a) => a

-- semiring1 :: forall a. Semiring a => a

-- equal1 = one :: forall a. Semiring a => Eq a => a

stringRenderer :: forall n c a x. Show a => Show x => Patch n c a x -> S.Signal String
stringRenderer (Patch _ patchSignal) =
    patchSignal S.~> (\item ->
        case item of
            Bang -> show "Bang"
            Data d -> show d
             -- make data items require a Show instance,
             -- maybe even everywhere. Also create some type class which defines interfaces
             -- for Node type and Channel type?
            Error x -> show ("Error: " <> (show x)))

send :: forall c a x. a -> Inlet c a x -> Inlet c a x
send v =
    attach (S.constant v)

sendError :: forall c a x. x -> Inlet c a x -> Inlet c a x
sendError e =
    attachErrors (S.constant e)

hello :: S.Signal String
hello = (ST.every 1000.0) S.~> show

helloEffect :: forall eff. S.Signal (Eff (console :: CONSOLE | eff) Unit)
helloEffect = hello S.~> log

main_ :: forall eff. Eff (console :: CONSOLE | eff) Unit
main_ = S.runSignal helloEffect

data MyNodeType = NumNode | StrNode

data MyInletType = NumInlet | StrInlet

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main =
    let
        patch = createPatch "foo"
        node = createNode "num" NumNode
        inlet = createInlet "foo" StrInlet
        nodeWithInlet = addInlet inlet node
        (Patch _ sumSignal) = addNode nodeWithInlet patch
    in
        -- send "5" inlet
        do
            S.runSignal ((stringRenderer patch) S.~> log)
            send "5" inlet
