module Blessed.UI.Lists.ListBar
    ( listbar
    , listbarAnd
    ) where


import Type.Row (type (+))


import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.BlessedSubj (Subject(..)) as Subject
import Blessed.UI.Lists.ListBar.Option (OptionsRow)
import Blessed.UI.Lists.ListBar.Event (Event)


listbar :: forall r. String -> C.Node ( OptionsRow + r ) Event
listbar name = C.node Subject.ListBar name


listbarAnd :: forall r. String -> C.NodeAnd ( OptionsRow + r ) Event
listbarAnd name = C.nodeAnd Subject.ListBar name
