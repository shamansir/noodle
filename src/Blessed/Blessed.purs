module Blessed where

import Prelude (($), Unit)

import Data.Function (apply, applyFlipped)

import Effect (Effect)

import Type.Row (type (+))
import Data.Symbol (class IsSymbol)


-- import Blessed.Internal.BlessedSubj (Subject(..)) as I
import Blessed.Internal.NodeKey (NodeKey)
import Blessed.Internal.Core (Blessed, Node, NodeAnd, run, runAnd) as C
import Blessed.Internal.Emitter (BlessedEvent) as C
import Blessed.Internal.Command (withProcess) as I
import Blessed.Internal.BlessedSubj (Screen, Box, Line, List, ListBar) as Subject
import Blessed.Internal.BlessedOp (BlessedOp, BlessedOp', performOnProcess) as I


-- import Blessed.UI.Base.Node (Node(..))
import Blessed.UI.Base.Screen (screen, screenAnd) as Screen
import Blessed.UI.Base.Screen.Option (OptionsRow) as Screen
import Blessed.UI.Boxes.Box (box, boxAnd) as Box
import Blessed.UI.Boxes.Box.Option (OptionsRow) as Box
import Blessed.UI.Boxes.Line (line, lineAnd) as Line
import Blessed.UI.Boxes.Line.Option (OptionsRow) as Line
import Blessed.UI.Lists.List (list, listAnd) as List
import Blessed.UI.Lists.List.Option (OptionsRow) as List
import Blessed.UI.Lists.ListBar (listbar, listbarAnd) as ListBar
import Blessed.UI.Lists.ListBar.Option (OptionsRow) as ListBar



import Data.Codec.Argonaut as CA


type Event = C.BlessedEvent



infixr 0 with_ as >~
-- type B e = {}
infixr 0 _with as ~<


-- ref :: String -> C.NodeId
-- ref id = I.NodeId id


run :: forall state. state -> C.Blessed state -> Effect Unit
run = C.run


runAnd :: forall state. state -> C.Blessed state -> I.BlessedOp state Effect -> Effect Unit
runAnd = C.runAnd


screen
    :: forall id r state
     . IsSymbol id
    => NodeKey Subject.Screen id
    -> C.Node Subject.Screen id ( Screen.OptionsRow + r ) state {- Screen.Event -}
screen = Screen.screen


screenAnd
    :: forall id r state
     . IsSymbol id
    => NodeKey Subject.Screen id
    -> C.NodeAnd Subject.Screen id ( Screen.OptionsRow + r ) state {- Screen.Event -}
screenAnd = Screen.screenAnd


box
    :: forall id r state
     . IsSymbol id
    => NodeKey Subject.Box id
    -> C.Node Subject.Box id ( Box.OptionsRow + r ) state {- Box.Event -}
box = Box.box


boxAnd
    :: forall id r state
     . IsSymbol id
    => NodeKey Subject.Box id
    -> C.NodeAnd Subject.Box id ( Box.OptionsRow + r ) state {- Box.Event -}
boxAnd = Box.boxAnd


line
    :: forall id r state
     . IsSymbol id
    => NodeKey Subject.Line id
    -> C.Node Subject.Line id ( Line.OptionsRow + r ) state {- Line.Event -}
line = Line.line


lineAnd
    :: forall id r state
     . IsSymbol id
    => NodeKey Subject.Line id
    -> C.NodeAnd Subject.Line id ( Line.OptionsRow + r ) state {- Line.Event -}
lineAnd = Line.lineAnd


list
    :: forall id r state
     . IsSymbol id
    => NodeKey Subject.List id
    -> C.Node Subject.List id ( Box.OptionsRow + List.OptionsRow + r ) state {- List.Event -}
list = List.list


listAnd
    :: forall id r state
     . IsSymbol id
    => NodeKey Subject.List id
    -> C.NodeAnd Subject.List id ( Box.OptionsRow + List.OptionsRow + r ) state {- List.Event -}
listAnd = List.listAnd


listbar
    :: forall id r state
     . IsSymbol id
    => NodeKey Subject.ListBar id
    -> C.Node Subject.ListBar id ( ListBar.OptionsRow + r ) state {- ListBar.Event -}
listbar = ListBar.listbar


listbarAnd
    :: forall id r state
     . IsSymbol id
    => NodeKey Subject.ListBar id
    -> C.NodeAnd Subject.ListBar id ( ListBar.OptionsRow + r ) state {- ListBar.Event -}
listbarAnd = ListBar.listbarAnd


exit :: forall state m. I.BlessedOp state m
exit = I.performOnProcess $ I.withProcess "exit" [ CA.encode CA.int 0 ]


failure :: forall state m. I.BlessedOp state m
failure = I.performOnProcess $ I.withProcess "exit" [ CA.encode CA.int 1 ]


with_ :: forall subj id state m. NodeKey subj id -> (NodeKey subj id -> I.BlessedOp state m) -> I.BlessedOp state m
with_ = applyFlipped


_with :: forall subj id state m a. (NodeKey subj id -> I.BlessedOp' state m a) -> NodeKey subj id -> I.BlessedOp' state m a
_with = apply