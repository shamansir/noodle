module Blessed.UI.Forms.TextArea
    ( textArea
    , textAreaAnd
    ) where


import Type.Row (type (+))
import Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(..))


import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.BlessedSubj (TextArea) as Subject
import Blessed.Internal.NodeKey (NodeKey)
import Blessed.UI.Boxes.Box.Option (OptionsRow) as Box
import Blessed.UI.Forms.TextArea.Option (OptionsRow)
import Blessed.UI.Forms.TextArea.Event (TextAreaEvent)


type TextArea id state r = C.Node Subject.TextArea id ( Box.OptionsRow + OptionsRow + r ) state
type TextAreaAnd id state r = C.NodeAnd Subject.TextArea id ( Box.OptionsRow + OptionsRow + r ) state


textArea :: forall id r state. IsSymbol id => NodeKey Subject.TextArea id -> TextArea id state r
textArea nodeKey = C.node nodeKey


textAreaAnd :: forall id r state. IsSymbol id => NodeKey Subject.TextArea id -> TextAreaAnd id state r
textAreaAnd nodeKey = C.nodeAnd ( Proxy :: _ TextAreaEvent ) nodeKey
