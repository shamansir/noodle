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

data NodeM m d =
    NodeM
        (Signal (String /\ d) /\ Signal (String /\ m d))


type Node = NodeM Identity

type EffNode = NodeM Effect

type AffNode = NodeM Aff


fromFn
    :: forall m d
     . Applicative m
    => d
    -> ((String -> Maybe d) -> m (String -> Maybe d))
    -> Effect { node :: NodeM m d, inlets :: Channel (String /\ d), outlets :: Channel (String /\ m d) }
fromFn def fn = do
    inlets_chan <- Ch.channel ("bang" /\ def)
    outlets_chan <- Ch.channel ("bang" /\ pure def)
    let
        inlets = Ch.subscribe inlets_chan
        outlets = Ch.subscribe outlets_chan
        node = NodeM (inlets /\ outlets)
    pure { node, inlets : inlets_chan, outlets : outlets_chan }

-- fromFn' :: (d -> d) -> Node''' d
