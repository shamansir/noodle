module Cli.Components.Console where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log) as Console


import Blessed.Internal.BlessedOp (BlessedOp)


log :: forall s m. MonadEffect m => String -> BlessedOp s m
log = const $ pure unit -- liftEffect <<< Console.log