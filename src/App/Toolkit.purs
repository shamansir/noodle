module App.Toolkit where


import Prelude (($), (<$>), (<*>), (<#>), (+), Unit, unit)

import Data.Tuple.Nested ((/\))


import Noodle.Node.Unit (Node)
import Noodle.Node.Unit (make) as Node
import Noodle.Node ((<+))
import Noodle.Node (pass', doNothing) as Node

import Effect (Effect)



library :: Effect (Node Unit)
library =
    Node.make
        unit
        Node.doNothing


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
