module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Signal as S
-- import Signal.Channel (CHANNEL, subscribe, send, channel)
import Signal.Time as ST

data Id = String

data Type = Tuple String String

type PatchModel a =
    { id :: Id
    , title :: String
    , nodes :: Array (Node a)
    , links :: Array (Link a)
    }

type NodeModel a =
    { id :: Id
    , title :: String
    , type :: Type
    , inlets :: Array (Inlet a)
    , outlets :: Array (Outlet a)
    }

type InletModel a =
    { id :: Id
    , label :: String
    , type :: Type
    }

type OutletModel a =
    { id :: Id
    , label :: String
    , type :: Type
    }

data Patch a = Patch (PatchModel a) (S.Signal a)

data Node a = Node (NodeModel a) (S.Signal a)

data Inlet a = Inlet (InletModel a) (S.Signal a)

data Outlet a = Outlet (OutletModel a) (S.Signal a)

data Link a = Link (Outlet a) (Inlet a)

connect :: forall a. Outlet a -> Inlet a -> Link a
connect outlet inlet =
    Link outlet inlet

hello :: S.Signal String
hello = (ST.every 1000.0) S.~> show

helloEffect :: forall eff. S.Signal (Eff (console :: CONSOLE | eff) Unit)
helloEffect = hello S.~> log

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = S.runSignal helloEffect
