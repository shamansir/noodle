module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Signal as S
import Signal.Channel (CHANNEL, subscribe, send, channel)

-- | A `Patch` is a `Signal` with a corresponding list of HTML elements
-- | for the user interface components.
data Patch a = Patch (Array Node) (S.Signal a)

-- | A `Node` is a `Signal` with a corresponding list of HTML elements
-- | for the user interface components.
data Node a = Node (Array Element) (S.Signal a)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
