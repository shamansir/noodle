module Blessed.UI.Boxes.Line
    ( line
    , lineAnd
    ) where


import Type.Row (type (+))
import Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(..))


import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.BlessedSubj (Line) as Subject
import Blessed.Internal.NodeKey (NodeKey)
import Blessed.UI.Boxes.Line.Option (OptionsRow)
import Blessed.UI.Boxes.Line.Event (LineEvent)


line :: forall id r state. IsSymbol id => NodeKey Subject.Line id -> C.Node Subject.Line id ( OptionsRow + r ) state
line nodeKey = C.node nodeKey


lineAnd :: forall id r state. IsSymbol id => NodeKey Subject.Line id -> C.NodeAnd Subject.Line id ( OptionsRow + r ) state
lineAnd nodeKey = C.nodeAnd ( Proxy :: _ LineEvent ) nodeKey