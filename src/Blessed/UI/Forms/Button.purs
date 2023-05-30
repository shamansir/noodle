module Blessed.UI.Forms.Button
    ( button
    , buttonAnd
    ) where


import Type.Row (type (+))
import Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(..))


import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.BlessedSubj (Button) as Subject
import Blessed.Internal.NodeKey (NodeKey)
import Blessed.UI.Boxes.Box.Option (OptionsRow) as Box
import Blessed.UI.Forms.Button.Option (OptionsRow)
import Blessed.UI.Forms.Button.Event (ButtonEvent)


type Button id state r = C.Node Subject.Button id ( Box.OptionsRow + OptionsRow + r ) state
type ButtonAnd id state r = C.NodeAnd Subject.Button id ( Box.OptionsRow + OptionsRow + r ) state


button :: forall id r state. IsSymbol id => NodeKey Subject.Button id -> Button id state r
button nodeKey = C.node nodeKey


buttonAnd :: forall id r state. IsSymbol id => NodeKey Subject.Button id -> ButtonAnd id state r
buttonAnd nodeKey = C.nodeAnd ( Proxy :: _ ButtonEvent ) nodeKey
