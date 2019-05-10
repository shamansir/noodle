module Rpd.IsData
    ( class IsData
    , default
    ) where


class IsData a where
    default :: a


-- class HasDefault a where
--     default :: a


-- class HasAlias a where
--     alias :: a


-- class (HasDefault a, HasAlias c) => Channel a c


-- class Channel d c where
--     alias :: forall d. c -> String
--     accept :: forall c. d -> Boolean
--     default :: c -> d

-- See Rpd.Toolkit
