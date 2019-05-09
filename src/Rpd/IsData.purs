module Rpd.IsData
    ( class IsData
    , default
    ) where


class IsData a where
    default :: a


-- class Channel d c where
--     alias :: forall d. c -> String
--     accept :: forall c. d -> Boolean
--     default :: c -> d

