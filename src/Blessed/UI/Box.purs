module Blessed.UI.Box
    {- ( module Blessed.UI.Box.Prop
    , module Blessed.UI.Box.Event
    , module Blessed.UI.Box.Op
    , box
    , boxAnd
    ) -} where


import Type.Row (type (+))

import Blessed.UI.Box.Prop (OptionsRow)
import Blessed.UI.Box.Event (Event)
import Blessed.UI.Box.Op


import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.JsApi (Kind(..)) as Kind



box :: forall r. String -> C.Node ( OptionsRow + r ) Event
box name = C.node Kind.Box name


boxAnd :: forall r. String -> C.NodeAnd ( OptionsRow + r ) Event
boxAnd name = C.nodeAnd Kind.Box name
