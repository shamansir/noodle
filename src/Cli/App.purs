module Cli.App where

import Prelude

import Effect.Class (class MonadEffect)


import Blessed.UI.Node (Node) as Blessed
import Blessed (BlessedOp, run, runAnd) as Blessed


run :: forall m. MonadEffect m => Blessed.Node -> m Unit
run = Blessed.run


runAnd :: forall m a. MonadEffect m => Blessed.Node -> Blessed.BlessedOp m a -> m Unit
runAnd = Blessed.runAnd