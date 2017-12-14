module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array ((:))
-- import Data.Monoid (mempty)
import Signal as S
import Signal.Time as ST

type Id = String

data Type = Tuple String String

type PatchModel a x =
    { id :: Id
    , title :: String
    , nodes :: Array (Node a x)
    , links :: Array (Link a x)
    }

type NodeModel =
    { id :: Id
    , title :: String
    , type :: Type
    , inlets :: Array InletModel
    , outlets :: Array OutletModel
    }

type InletModel =
    { id :: Id
    , label :: String
    , type :: Type
    }

type OutletModel =
    { id :: Id
    , label :: String
    , type :: Type
    }

data Flow a x = Bang | Data a | Error x

data Patch a x = Patch (PatchModel a x) (S.Signal (Flow a x))

data Node a x = Node NodeModel (S.Signal (Flow a x))

data Inlet a x = Inlet InletModel (S.Signal (Flow a x))

data Outlet a x = Outlet OutletModel (S.Signal (Flow a x))

data Link a x = Link (Outlet a x) (Inlet a x)

createPatch :: forall a x. String -> Patch a x
createPatch title =
    Patch
        { id : "test"
        , title : title
        , nodes : []
        , links : []
        }
        (S.constant Bang)

createNode :: forall a x. String -> Type -> Node a x
createNode title nodeType =
    Node
        { id : "test"
        , title : title
        , type : nodeType
        , inlets : []
        , outlets : []
        }
        (S.constant Bang)

createInlet :: forall a x. String -> Type -> Inlet a x
createInlet label inletType =
    Inlet
        { id : "test"
        , label : label
        , type : inletType
        }
        (S.constant Bang)

createOutlet :: forall a x. String -> Type -> Outlet a x
createOutlet label outletType =
    Outlet
        { id : "test"
        , label : label
        , type : outletType
        }
        (S.constant Bang)

connect :: forall a x. Outlet a x -> Inlet a x -> Link a x
connect outlet inlet =
    Link outlet inlet

addInlet :: forall a x. Inlet a x -> Node a x -> Node a x
-- addInlet inlet'@(Inlet inlet inletSignal) (Node node nodeSignal) =
--  Node (node { inlets = inlet' : node.inlets }) (S.merge nodeSignal inletSignal)
addInlet (Inlet inlet inletSignal) (Node node nodeSignal) =
    Node (node { inlets = inlet : node.inlets }) (S.merge nodeSignal inletSignal)

addOutlet :: forall a x. Outlet a x -> Node a x -> Node a x
addOutlet (Outlet outlet outletSignal) (Node node nodeSignal) =
    Node (node { outlets = outlet : node.outlets }) (S.merge nodeSignal outletSignal)

hello :: S.Signal String
hello = (ST.every 1000.0) S.~> show

helloEffect :: forall eff. S.Signal (Eff (console :: CONSOLE | eff) Unit)
helloEffect = hello S.~> log

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = S.runSignal helloEffect
