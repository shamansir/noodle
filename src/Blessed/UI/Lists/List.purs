module Blessed.UI.Lists.List
    ( list
    , listAnd
    ) where


import Type.Row (type (+))


import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.BlessedKind (NKind(..)) as Kind
import Blessed.UI.Lists.List.Option (OptionsRow)
import Blessed.UI.Lists.List.Event (Event)


list :: forall r. String -> C.Node ( OptionsRow + r ) Event
list name = C.node Kind.List name


listAnd :: forall r. String -> C.NodeAnd ( OptionsRow + r ) Event
listAnd name = C.nodeAnd Kind.List name