module Blessed.UI.Boxes.Line
    ( line
    , lineAnd
    ) where


import Type.Row (type (+))


import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.BlessedSubj (Subject(..)) as Subject
import Blessed.UI.Boxes.Line.Option (OptionsRow)
import Blessed.UI.Boxes.Line.Event (Event)


line :: forall r. String -> C.Node ( OptionsRow + r ) Event
line name = C.node Subject.Line name


lineAnd :: forall r. String -> C.NodeAnd ( OptionsRow + r ) Event
lineAnd name = C.nodeAnd Subject.Line name