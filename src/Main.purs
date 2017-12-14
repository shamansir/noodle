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

type PatchModel n c a x =
    { id :: Id
    , title :: String
    , nodes :: Array (Node n c a x)
    , links :: Array (Link c a x)
    }

type NodeModel n c =
    { id :: Id
    , title :: String
    , type :: n
    , inlets :: Array (InletModel c)
    , outlets :: Array (OutletModel c)
    }

type InletModel c =
    { id :: Id
    , label :: String
    , type :: c
    }

type OutletModel c =
    { id :: Id
    , label :: String
    , type :: c
    }

data Flow a x = Bang | Data a | Error x

data Patch n c a x = Patch (PatchModel n c a x) (S.Signal (Flow a x))

data Node n c a x = Node (NodeModel n c) (S.Signal (Flow a x))

data Inlet c a x = Inlet (InletModel c) (S.Signal (Flow a x))

data Outlet c a x = Outlet (OutletModel c) (S.Signal (Flow a x))

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

addInlet :: forall n c a x. Inlet c a x -> Node n c a x -> Node n c a x
-- addInlet inlet'@(Inlet inlet inletSignal) (Node node nodeSignal) =
--  Node (node { inlets = inlet' : node.inlets }) (S.merge nodeSignal inletSignal)
addInlet (Inlet inlet inletSignal) (Node node nodeSignal) =
    Node (node { inlets = inlet : node.inlets }) (S.merge nodeSignal inletSignal)

addOutlet :: forall n c a x. Outlet c a x -> Node n c a x -> Node n c a x
addOutlet (Outlet outlet outletSignal) (Node node nodeSignal) =
    Node (node { outlets = outlet : node.outlets }) (S.merge nodeSignal outletSignal)

hello :: S.Signal String
hello = (ST.every 1000.0) S.~> show

helloEffect :: forall eff. S.Signal (Eff (console :: CONSOLE | eff) Unit)
helloEffect = hello S.~> log

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = S.runSignal helloEffect
