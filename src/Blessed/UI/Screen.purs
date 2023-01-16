module Blessed.UI.Screen
    ( module Blessed.UI.Screen.Property
    , module Blessed.UI.Screen.Event
    , module Blessed.UI.Screen.Method
    , screen
    , screenAnd
    ) where


import Type.Row (type (+))


import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.JsApi (Kind(..)) as Kind

import Blessed.UI.Screen.Property
import Blessed.UI.Screen.Event
import Blessed.UI.Screen.Method



screen :: forall r. String -> C.Node ( PropertiesRow + r ) Event
screen name = C.node Kind.Screen name


screenAnd :: forall r. String -> C.NodeAnd ( PropertiesRow + r ) Event
screenAnd name = C.nodeAnd Kind.Screen name
