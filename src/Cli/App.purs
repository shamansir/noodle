module Cli.App where

import Prelude

import Effect.Class (class MonadEffect)


import Blessed.UI.Node (Node) as Blessed
import Blessed.Internal.Core (Blessed, BlessedOp)
import Blessed (Event, run, runAnd) as Blessed


run :: forall m. MonadEffect m => Blessed m Blessed.Event -> m Unit
run = Blessed.run


runAnd :: forall m a. MonadEffect m => Blessed m Blessed.Event -> BlessedOp m a -> m Unit
runAnd = Blessed.runAnd