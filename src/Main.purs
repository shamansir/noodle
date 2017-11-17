module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Tuple
import Signal as S
import Signal.Channel (CHANNEL, subscribe, send, channel)

-- | A `Patch` is a `Signal` with a corresponding list of HTML elements
-- | for the user interface components.
data Patch a = Patch (Array (Node a)) (S.Signal a)

-- | A `Node` is a `Signal` with a corresponding list of HTML elements
-- | for the user interface components.
data Node a = Node (Tuple (Array (Inlet a)) (Array (Outlet a))) (S.Signal a)

data Inlet a = Inlet (S.Signal a)

data Outlet a = Outlet (S.Signal a)

data Link a = Link (Tuple (Outlet a) (Inlet a))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
