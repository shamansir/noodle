module Node where

import Prelude (bind, pure, ($), (#), flip, (<$>), (>>>), (<<<), (>>=), (=<<), unit, Unit)

import Control.Applicative (class Applicative, class Apply)

import Data.Array ((..))
import Data.Identity (Identity)
import Data.Maybe (Maybe)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (unwrap, class Newtype)
import Data.Traversable (traverse, sequence)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Signal (Signal)
import Signal (foldp, unwrap, flatten, constant) as Signal
import Signal.Channel (Channel)
import Signal.Channel as Ch


-- data Node' a m d = Node' (Channel d /\ Channel (m d)) a


-- type Node'' m d = Node' ( Array String /\ Array String ) m ( String /\ d )


-- type Node''' d = Node' Identity Unit

data Node d =
    Node
        (Signal (String /\ d) /\ Signal (Effect (String /\ d)))


--type Node = NodeM Identity


type NodeDef d = { node :: Node d, in :: Channel (String /\ d), out :: Channel (Effect (String /\ d)) }


newtype Receive d = Receive (Map String d)


newtype Send d = Send (Map String d)


fromFn
    :: forall d
     . d
    -> (Receive d -> Effect (Send d))
    -> Effect (NodeDef d)
fromFn def fn = do
    inlets_chan <- Ch.channel ("bang" /\ def)
    outlets_chan <- Ch.channel (pure $ "bang" /\ def)
    let
        inlets = Ch.subscribe inlets_chan
        outlets = Ch.subscribe outlets_chan
        node = Node (inlets /\ outlets)
        --maps :: Signal (Map String d)
        maps = inlets # Signal.foldp (uncurry Map.insert) Map.empty
        --fn_signal :: Signal (Effect (Send d))
        fn_signal = (Receive >>> fn) <$> maps
        sendFx :: Signal (Effect (Array (Effect Unit)))
        sendFx =
            ((flip (>>=))
                \(Send map) ->
                    pure $ (Ch.send outlets_chan <<< pure) <$> Map.toUnfoldable map
            ) <$> fn_signal
        out_signal :: Effect (Signal Unit)
        out_signal =
            liftEffect
                =<< Signal.unwrap
                <$> (\sig -> Signal.flatten sig $ pure unit)
                <$> Signal.unwrap sendFx
    _ <- out_signal
    pure { node, in : inlets_chan, out : outlets_chan }

-- fromFn' :: (d -> d) -> Node''' d


receive :: forall d. String -> Receive d -> Maybe d
receive label (Receive r) = Map.lookup label r -- unwrap >>> flip Map.lookup