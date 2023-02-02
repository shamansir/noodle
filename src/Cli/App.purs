module Cli.App where

import Prelude

import Effect (Effect)


import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.Core (Blessed)
import Blessed (Event, run, runAnd) as Blessed


run :: forall state. state -> Blessed state Blessed.Event -> Effect Unit
run = Blessed.run


runAnd :: forall state. state -> Blessed Blessed.Event -> BlessedOp state Effect -> Effect Unit
runAnd = Blessed.runAnd