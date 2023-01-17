module Blessed.UI.Screen
    ( screen
    , screenAnd
    ) where


import Type.Row (type (+))


import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.JsApi (Kind(..)) as Kind
import Blessed.UI.Screen.Option (OptionsRow)
import Blessed.UI.Screen.Event (Event)


screen :: forall r. String -> C.Node ( OptionsRow + r ) Event
screen name = C.node Kind.Screen name


screenAnd :: forall r. String -> C.NodeAnd ( OptionsRow + r ) Event
screenAnd name = C.nodeAnd Kind.Screen name
