module Node where

import Control.Applicative (class Applicative, class Apply)
import Data.Array ((..))
import Data.Identity (Identity)
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Prelude (bind, pure)
import Signal (Signal)
import Signal.Channel (Channel)
import Signal.Channel as Ch


-- data Node' a m d = Node' (Channel d /\ Channel (m d)) a


-- type Node'' m d = Node' ( Array String /\ Array String ) m ( String /\ d )


-- type Node''' d = Node' Identity Unit

data Node m d =
    Node
        (Channel (String /\ d) /\ Channel (String /\ m d))


--type Node = NodeM Identity

type EffNode = Node Effect

type AffNode = Node Aff


fromFn
    :: forall m d
     . Applicative m
    => d
    -> ((String -> Maybe d) -> m (String -> Maybe d))
    -> Effect (Node m d)
fromFn def fn = do
    inlets_chan <- Ch.channel ("bang" /\ def)
    outlets_chan <- Ch.channel ("bang" /\ pure def)
    let
        -- inlets = Ch.subscribe inlets_chan
        -- outlets = Ch.subscribe outlets_chan
        node = Node (inlets_chan /\ outlets_chan)
    pure node

-- fromFn' :: (d -> d) -> Node''' d
