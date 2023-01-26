module Blessed.UI.Base.Screen
    ( screen
    , screenAnd
    ) where


import Type.Row (type (+))


import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.BlessedSubj (Subject(..)) as Subject
import Blessed.UI.Base.Screen.Option (OptionsRow)
import Blessed.UI.Base.Screen.Event (Event)


screen :: forall r. String -> C.Node ( OptionsRow + r ) Event
screen name = C.node Subject.Screen name


screenAnd :: forall r. String -> C.NodeAnd ( OptionsRow + r ) Event
screenAnd name = C.nodeAnd Subject.Screen name
