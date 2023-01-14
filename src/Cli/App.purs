module Cli.App where

import Prelude

import Effect.Class (class MonadEffect)


import Blessed.UI.Node (Node) as Blessed
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.Core (Blessed)
import Blessed (Event, run, runAnd) as Blessed


run :: forall m. MonadEffect m => Blessed m Blessed.Event -> m Unit
run = Blessed.run


runAnd :: forall m. MonadEffect m => Blessed m Blessed.Event -> BlessedOp m -> m Unit
runAnd = Blessed.runAnd