module Noodle.Channel where


import Data.Maybe (Maybe)


data Temperature
    = Hot
    | Cold


data Visibility
    = Hidden
    | Visible


{-
class Channel d where
    --adapt :: d -> d
    accept :: d -> Maybe d
    id :: forall d. String


type FullDef d =
    { id :: String
    , default :: d -- d?
    --, accept :: d -> Maybe a -- TODO: remove, move to typeclass, can be used on `Node.connect` / `Node.send`
    --, adapt :: d -> d
    , temperature :: Temperature
    , visibility :: Visibility
    }
-}