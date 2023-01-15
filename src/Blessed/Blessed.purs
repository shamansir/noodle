module Blessed where

import Prelude
import Foreign (Foreign)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Data.Map (Map)
import Data.Maybe (Maybe)
import Type.Row (type (+))


-- import Blessed.UI.Node (Node(..))
import Blessed.UI.Node as Node
import Blessed.UI.Screen as Screen
import Blessed.UI.Box as Box
import Blessed.Internal.Core as C
import Blessed.Internal.Command (NodeId, withProcess) as I
import Blessed.Internal.BlessedOp as I


type Event = C.CoreEvent


type State m e =
    { nodes :: Map I.NodeId (C.Blessed m e)
    , lastId :: Int
    }



-- type B e = {}


run :: forall m. MonadEffect m => C.Blessed m Event -> m Unit
run = liftEffect <<< C.execute_


runAnd :: forall m. MonadEffect m => C.Blessed m Event -> I.BlessedOp m -> m Unit
runAnd _ _ = pure unit


screen :: forall r m. String -> C.Node ( Screen.OptionsRow + r ) m Screen.Event
screen = Screen.screen


screenAnd :: forall r m. String -> C.NodeAnd ( Screen.OptionsRow + r ) m Screen.Event
screenAnd = Screen.screenAnd


box :: forall r m. String -> C.Node ( Box.OptionsRow + r ) m Box.Event
box = Box.box


boxAnd :: forall r m. String -> C.NodeAnd ( Box.OptionsRow + r ) m Box.Event
boxAnd = Box.boxAnd


exit :: forall m. I.BlessedOp m
exit = I.performOnProcess $ I.withProcess "exit" []


with_ :: forall m. String -> (C.NodeId -> I.BlessedOp m) -> I.BlessedOp m
with_ id fn = pure unit -- FIXME: TODO