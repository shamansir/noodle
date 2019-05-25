module Rpd.Channel
    ( class Channel
    , default, accept, adapt, show
    ) where

import Prelude (class Show)

-- FIXME: move to Toolkit module
-- FIXME: the name "Channel" is not right, it's rather Channels system... `ChannelDef`?
class (Show c) <= Channel c d where
    default :: c -> d
    accept :: c -> d -> Boolean
    adapt :: c -> d -> d
    -- repr :: forall x. Show x => c -> d -> x
    show :: c -> d -> String
