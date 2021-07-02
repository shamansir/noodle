module App.Toolkit where


import Prelude (($), (<$>), (<*>), (<#>), (+))

import Data.Tuple.Nested ((/\))


import Noodle.Node.Unit (Node)
import Noodle.Node.Unit (make) as Node
import Noodle.Node ((<+))
import Noodle.Node (pass') as Node

import Effect (Effect)


sumNode :: Effect (Node Int)
sumNode =
    Node.make 0
      $ \inlets ->
          Node.pass'
            [ "c" /\ ((+) <$> "a" <+ inlets
                          <*> "b" <+ inlets
                     )
            ]

-- TODO


-- timerNode -- TODO


-- sineNode -- TODO
