module Noodle.Node.Walk where

import Noodle.Node.Path (InNode)


class Walk a repr where
    visit :: forall f i o. InNode f i o -> a -> repr -- include Repr as kind here?