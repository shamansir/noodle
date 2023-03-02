module Cli.App where

import Prelude

import Effect (Effect)


import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.Core (Blessed)
import Blessed (run, runAnd) as Blessed


run :: forall state. state -> Blessed state -> Effect Unit
run = Blessed.run


runAnd :: forall state. state -> Blessed state -> BlessedOp state Effect -> Effect Unit
runAnd = Blessed.runAnd