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
import Blessed.UI.Boxes.Box.Option (OptionsRow) as Input
import Blessed.UI.Forms.Button.Option (OptionsRow)
import Blessed.UI.Forms.Button.Event (ButtonEvent)


button :: forall id r state. IsSymbol id => NodeKey Subject.Button id -> C.Node Subject.Button id ( Input.OptionsRow + OptionsRow + r ) state
button nodeKey = C.node nodeKey


buttonAnd :: forall id r state. IsSymbol id => NodeKey Subject.Button id -> C.NodeAnd Subject.Button id ( Input.OptionsRow + OptionsRow + r ) state
buttonAnd nodeKey = C.nodeAnd ( Proxy :: _ ButtonEvent ) nodeKey
