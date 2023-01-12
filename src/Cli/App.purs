module Cli.App where

import Prelude

import Effect.Class (class MonadEffect)


import Blessed as Blessed


run :: forall m a. MonadEffect m => Blessed.Node -> Blessed.BlessedOp m a -> m Unit
run = Blessed.run