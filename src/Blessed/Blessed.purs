module Blessed where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)

import Type.Row (type (+))


-- import Blessed.UI.Node (Node(..))
import Blessed.UI.Screen (screen, screenAnd) as Screen
import Blessed.UI.Screen.Property (PropertiesRow) as Screen
import Blessed.UI.Screen.Event (Event) as Screen
import Blessed.UI.Box (box, boxAnd) as Box
import Blessed.UI.Box.Property (PropertiesRow) as Box
import Blessed.UI.Box.Event (Event) as Box
import Blessed.Internal.Core as C
import Blessed.Internal.Command (withProcess) as I
import Blessed.Internal.BlessedOp (BlessedOp, execute_, performOnProcess) as I


type Event = C.CoreEvent



-- type B e = {}


run :: C.Blessed Event -> Effect Unit
run = liftEffect <<< I.execute_ <<< C.encode


runAnd :: C.Blessed Event -> I.BlessedOp Effect -> Effect Unit
runAnd _ _ = pure unit


screen :: forall r. String -> C.Node ( Screen.PropertiesRow + r ) Screen.Event
screen = Screen.screen


screenAnd :: forall r. String -> C.NodeAnd ( Screen.PropertiesRow + r ) Screen.Event
screenAnd = Screen.screenAnd


box :: forall r. String -> C.Node ( Box.PropertiesRow + r ) Box.Event
box = Box.box


boxAnd :: forall r. String -> C.NodeAnd ( Box.PropertiesRow + r ) Box.Event
boxAnd = Box.boxAnd


exit :: forall m. I.BlessedOp m
exit = I.performOnProcess $ I.withProcess "exit" []


with_ :: forall m. String -> (C.NodeId -> I.BlessedOp m) -> I.BlessedOp m
with_ id fn = pure unit -- FIXME: TODO