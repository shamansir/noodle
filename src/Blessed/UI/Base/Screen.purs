module Blessed.UI.Base.Screen
    ( screen
    , screenAnd
    ) where


import Type.Row (type (+))
import Data.Symbol (class IsSymbol)

import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.BlessedSubj (Screen) as Subject
import Blessed.Internal.NodeKey (NodeKey)
import Blessed.UI.Base.Screen.Option (OptionsRow)
import Blessed.UI.Base.Screen.Event (Event)


screen :: forall id r. IsSymbol id => NodeKey Subject.Screen id -> C.Node Subject.Screen id ( OptionsRow + r ) Event
screen nodeKey = C.node nodeKey


screenAnd :: forall id r. IsSymbol id => NodeKey Subject.Screen id -> C.NodeAnd Subject.Screen id ( OptionsRow + r ) Event
screenAnd nodeKey = C.nodeAnd nodeKey
