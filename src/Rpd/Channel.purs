module Rpd.Channel
    ( class Channel
    , default, accept, adapt, show
    ) where

import Prelude (class Show)

class (Show c) <= Channel c d where
    default :: c -> d
    accept :: c -> d -> Boolean
    adapt :: c -> d -> d
    show :: forall x. Show x => c -> d -> x
