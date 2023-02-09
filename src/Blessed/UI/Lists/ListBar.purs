module Blessed.UI.Lists.ListBar
    ( listbar
    , listbarAnd
    ) where


import Type.Row (type (+))
import Data.Symbol (class IsSymbol)


import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.BlessedSubj (ListBar) as Subject
import Blessed.Internal.NodeKey (NodeKey)
import Blessed.UI.Lists.ListBar.Option (OptionsRow)
import Blessed.UI.Lists.ListBar.Event (ListBarEvent)


listbar :: forall id r state. IsSymbol id => NodeKey Subject.ListBar id -> C.Node Subject.ListBar id ( OptionsRow + r ) state ListBarEvent
listbar nodeKey = C.node nodeKey


listbarAnd :: forall id r state. IsSymbol id => NodeKey Subject.ListBar id -> C.NodeAnd Subject.ListBar id ( OptionsRow + r ) state ListBarEvent
listbarAnd nodeKey = C.nodeAnd nodeKey
