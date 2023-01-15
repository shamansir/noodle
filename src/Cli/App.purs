module Cli.App where

import Prelude

import Effect (Effect)


import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.Core (Blessed)
import Blessed (Event, run, runAnd) as Blessed


run :: Blessed Blessed.Event -> Effect Unit
run = Blessed.run


runAnd :: Blessed Blessed.Event -> BlessedOp Effect -> Effect Unit
runAnd = Blessed.runAnd