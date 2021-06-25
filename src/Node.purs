module Node where

import Prelude (bind, pure, ($), (#), flip, (<$>))

import Control.Applicative (class Applicative, class Apply)

import Data.Array ((..))
import Data.Identity (Identity)
import Data.Maybe (Maybe)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Map (Map)
import Data.Map as Map

import Effect (Effect)
import Effect.Aff (Aff)

import Signal (Signal)
import Signal (foldp) as Signal
import Signal.Channel (Channel)
import Signal.Channel as Ch


-- data Node' a m d = Node' (Channel d /\ Channel (m d)) a


-- type Node'' m d = Node' ( Array String /\ Array String ) m ( String /\ d )


-- type Node''' d = Node' Identity Unit

data Node m d =
    Node
        (Signal (String /\ d) /\ Signal (m (String /\ d)))


--type Node = NodeM Identity

type EffNode = Node Effect

type AffNode = Node Aff


type NodeDef m d = { node :: Node m d, in :: Channel (String /\ d), out :: Channel (m (String /\ d)) }


-- data Receive d = Receive (Map String d)

-- data Send d = Send (Map String d)


fromFn
    :: forall m d
     . Applicative m
    => d
    -> ((String -> Maybe d) -> m (String -> Maybe d))
    -> Effect (NodeDef m d)
fromFn def fn = do
    inlets_chan <- Ch.channel ("bang" /\ def)
    outlets_chan <- Ch.channel (pure $ "bang" /\ def)
    let
        inlets = Ch.subscribe inlets_chan
        outlets = Ch.subscribe outlets_chan
        node = Node (inlets /\ outlets)
        maps :: Signal (Map String d)
        maps = inlets # Signal.foldp (uncurry Map.insert) Map.empty
        fn_signal :: Signal (m (String -> Maybe d))
        fn_signal = (\values -> fn (flip Map.lookup $ values)) <$> maps
    pure { node, in : inlets_chan, out : outlets_chan }

-- fromFn' :: (d -> d) -> Node''' d
