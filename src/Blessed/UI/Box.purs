module Blessed.UI.Box
    ( box
    , boxAnd
    ) where


import Type.Row (type (+))

import Blessed.UI.Box.Option (OptionsRow)
import Blessed.UI.Box.Event (Event)


import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.JsApi (Kind(..)) as Kind



box :: forall r. String -> C.Node ( OptionsRow + r ) Event
box name = C.node Kind.Box name


boxAnd :: forall r. String -> C.NodeAnd ( OptionsRow + r ) Event
boxAnd name = C.nodeAnd Kind.Box name
