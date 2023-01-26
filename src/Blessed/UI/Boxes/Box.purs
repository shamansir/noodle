module Blessed.UI.Boxes.Box
    ( box
    , boxAnd
    ) where


import Type.Row (type (+))

import Blessed.UI.Boxes.Box.Option (OptionsRow)
import Blessed.UI.Boxes.Box.Event (Event)


import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.BlessedSubj (Subject(..)) as Subject



box :: forall r. String -> C.Node ( OptionsRow + r ) Event
box name = C.node Subject.Box name


boxAnd :: forall r. String -> C.NodeAnd ( OptionsRow + r ) Event
boxAnd name = C.nodeAnd Subject.Box name
