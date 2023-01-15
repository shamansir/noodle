module Blessed where

import Prelude
import Foreign (Foreign)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

import Data.Map (Map)
import Data.Maybe (Maybe)
import Type.Row (type (+))


-- import Blessed.UI.Node (Node(..))
import Blessed.UI.Screen as Screen
import Blessed.UI.Box as Box
import Blessed.Internal.Core as C
import Blessed.Internal.Command (withProcess) as I
import Blessed.Internal.JsApi (NodeId) as I
import Blessed.Internal.BlessedOp as I


type Event = C.CoreEvent



-- type B e = {}


run :: C.Blessed Event -> Effect Unit
run = liftEffect <<< I.execute_ <<< C.encode


runAnd :: C.Blessed Event -> I.BlessedOp Effect -> Effect Unit
runAnd _ _ = pure unit


screen :: forall r. String -> C.Node ( Screen.OptionsRow + r ) Screen.Event
screen = Screen.screen


screenAnd :: forall r. String -> C.NodeAnd ( Screen.OptionsRow + r ) Screen.Event
screenAnd = Screen.screenAnd


box :: forall r. String -> C.Node ( Box.OptionsRow + r ) Box.Event
box = Box.box


boxAnd :: forall r. String -> C.NodeAnd ( Box.OptionsRow + r ) Box.Event
boxAnd = Box.boxAnd


exit :: forall m. I.BlessedOp m
exit = I.performOnProcess $ I.withProcess "exit" []


with_ :: forall m. String -> (C.NodeId -> I.BlessedOp m) -> I.BlessedOp m
with_ id fn = pure unit -- FIXME: TODO