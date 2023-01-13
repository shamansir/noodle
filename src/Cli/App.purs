module Cli.App where

import Prelude

import Effect.Class (class MonadEffect)


import Blessed.UI.Node (Node) as Blessed
import Blessed (Blessed)
import Blessed (Blessed, Event, BlessedOp, run, runAnd) as Blessed


run :: forall m. MonadEffect m => Blessed m Blessed.Event -> m Unit
run = Blessed.run


runAnd :: forall m a. MonadEffect m => Blessed m Blessed.Event -> Blessed.BlessedOp m a -> m Unit
runAnd = Blessed.runAnd