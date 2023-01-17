module Blessed where

import Prelude

import Data.Function (applyFlipped)

import Effect (Effect)
import Effect.Class (liftEffect)

import Type.Row (type (+))


-- import Blessed.UI.Node (Node(..))
import Blessed.UI.Screen (screen, screenAnd) as Screen
import Blessed.UI.Screen.Option (OptionsRow) as Screen
import Blessed.UI.Screen.Event (Event) as Screen
import Blessed.UI.Box (box, boxAnd) as Box
import Blessed.UI.Box.Option (OptionsRow) as Box
import Blessed.UI.Box.Event (Event) as Box
import Blessed.Internal.JsApi (NodeId(..)) as I
import Blessed.Internal.Core (Blessed, Node, NodeAnd, NodeId, encode) as C
import Blessed.Internal.Emitter (CoreEvent) as C
import Blessed.Internal.Command (withProcess) as I
import Blessed.Internal.BlessedOp (BlessedOp, execute_, performOnProcess) as I

import Data.Codec.Argonaut as CA


type Event = C.CoreEvent



infixr 0 with_ as >~
-- type B e = {}


ref :: String -> C.NodeId
ref = I.NodeId


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
exit = I.performOnProcess $ I.withProcess "exit" [ CA.encode CA.int 0 ]


failure :: forall m. I.BlessedOp m
failure = I.performOnProcess $ I.withProcess "exit" [ CA.encode CA.int 1 ]


with_ :: forall m. C.NodeId -> (C.NodeId -> I.BlessedOp m) -> I.BlessedOp m
with_ = applyFlipped