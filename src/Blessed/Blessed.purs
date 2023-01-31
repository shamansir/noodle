module Blessed where

import Prelude

import Data.Function (applyFlipped)
import Data.Tuple.Nested ((/\))

import Effect (Effect)
import Effect.Class (liftEffect)

import Type.Row (type (+))
import Data.Symbol (class IsSymbol)


-- import Blessed.UI.Base.Node (Node(..))
import Blessed.UI.Base.Screen (screen, screenAnd) as Screen
import Blessed.UI.Base.Screen.Option (OptionsRow) as Screen
import Blessed.UI.Base.Screen.Event (Event) as Screen
import Blessed.UI.Boxes.Box (box, boxAnd) as Box
import Blessed.UI.Boxes.Box.Option (OptionsRow) as Box
import Blessed.UI.Boxes.Box.Event (Event) as Box
import Blessed.UI.Boxes.Line (line, lineAnd) as Line
import Blessed.UI.Boxes.Line.Option (OptionsRow) as Line
import Blessed.UI.Boxes.Line.Event (Event) as Line
import Blessed.UI.Lists.List (list, listAnd) as List
import Blessed.UI.Lists.List.Option (OptionsRow) as List
import Blessed.UI.Lists.List.Event (Event) as List
import Blessed.UI.Lists.ListBar (listbar, listbarAnd) as ListBar
import Blessed.UI.Lists.ListBar.Option (OptionsRow) as ListBar
import Blessed.UI.Lists.ListBar.Event (Event) as ListBar
-- import Blessed.Internal.BlessedSubj (Subject(..)) as I
import Blessed.Internal.NodeKey (NodeKey)
import Blessed.Internal.Core (Blessed, Node, NodeAnd, encode) as C
import Blessed.Internal.Emitter (CoreEvent) as C
import Blessed.Internal.Command (withProcess) as I
import Blessed.Internal.BlessedSubj (Screen, Box, Line, List, ListBar) as Subject
import Blessed.Internal.BlessedOp (BlessedOp, execute_, performOnProcess) as I

import Data.Codec.Argonaut as CA


type Event = C.CoreEvent



infixr 0 with_ as >~
-- type B e = {}


-- ref :: String -> C.NodeId
-- ref id = I.NodeId id


run :: C.Blessed Event -> Effect Unit
run = liftEffect <<< I.execute_ <<< C.encode


runAnd :: C.Blessed Event -> I.BlessedOp Effect -> Effect Unit
runAnd _ _ = pure unit


screen
    :: forall id r
     . IsSymbol id
    => NodeKey Subject.Screen id
    -> C.Node Subject.Screen id ( Screen.OptionsRow + r ) Screen.Event
screen = Screen.screen


screenAnd
    :: forall id r
     . IsSymbol id
    => NodeKey Subject.Screen id
    -> C.NodeAnd Subject.Screen id ( Screen.OptionsRow + r ) Screen.Event
screenAnd = Screen.screenAnd


box
    :: forall id r
     . IsSymbol id
    => NodeKey Subject.Box id
    -> C.Node Subject.Box id ( Box.OptionsRow + r ) Box.Event
box = Box.box


boxAnd
    :: forall id r
     . IsSymbol id
    => NodeKey Subject.Box id
    -> C.NodeAnd Subject.Box id ( Box.OptionsRow + r ) Box.Event
boxAnd = Box.boxAnd


line
    :: forall id r
     . IsSymbol id
    => NodeKey Subject.Line id
    -> C.Node Subject.Line id ( Line.OptionsRow + r ) Line.Event
line = Line.line


lineAnd
    :: forall id r
     . IsSymbol id
    => NodeKey Subject.Line id
    -> C.NodeAnd Subject.Line id ( Line.OptionsRow + r ) Line.Event
lineAnd = Line.lineAnd


list
    :: forall id r
     . IsSymbol id
    => NodeKey Subject.List id
    -> C.Node Subject.List id ( List.OptionsRow + r ) List.Event
list = List.list


listAnd
    :: forall id r
     . IsSymbol id
    => NodeKey Subject.List id
    -> C.NodeAnd Subject.List id ( List.OptionsRow + r ) List.Event
listAnd = List.listAnd


listbar
    :: forall id r
     . IsSymbol id
    => NodeKey Subject.ListBar id
    -> C.Node Subject.ListBar id ( ListBar.OptionsRow + r ) ListBar.Event
listbar = ListBar.listbar


listbarAnd
    :: forall id r
     . IsSymbol id
    => NodeKey Subject.ListBar id
    -> C.NodeAnd Subject.ListBar id ( ListBar.OptionsRow + r ) ListBar.Event
listbarAnd = ListBar.listbarAnd


exit :: forall m. I.BlessedOp m
exit = I.performOnProcess $ I.withProcess "exit" [ CA.encode CA.int 0 ]


failure :: forall m. I.BlessedOp m
failure = I.performOnProcess $ I.withProcess "exit" [ CA.encode CA.int 1 ]


with_ :: forall subj id m. NodeKey subj id -> (NodeKey subj id -> I.BlessedOp m) -> I.BlessedOp m
with_ = applyFlipped