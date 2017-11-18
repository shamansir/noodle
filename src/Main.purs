module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Tuple
import Signal as S
import Signal.Channel (CHANNEL, subscribe, send, channel)

data Type = Tuple String String

type PatchModel a =
    { id :: String
    , title :: String
    , nodes :: Array (Node a)
    , links :: Array (Link a)
    }

type NodeModel a =
    { id :: String
    , title :: String
    , type :: Type
    , inlets :: Array (Inlet a)
    , outlets :: Array (Outlet a)
    }

type InletModel a =
    { id :: String
    , label :: String
    , type :: Type
    }

type OutletModel a =
    { id :: String
    , label :: String
    , type :: Type
    }

data Patch a = Patch (PatchModel a) (S.Signal a)

data Node a = Node (NodeModel a) (S.Signal a)

data Inlet a = Inlet (InletModel a) (S.Signal a)

data Outlet a = Outlet (OutletModel a) (S.Signal a)

data Link a = Link (Tuple (Outlet a) (Inlet a))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
