module Blessed.UI.Forms.TextBox
    ( textBox
    , textBoxAnd
    ) where


import Type.Row (type (+))
import Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(..))


import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.BlessedSubj (TextBox) as Subject
import Blessed.Internal.NodeKey (NodeKey)
import Blessed.UI.Boxes.Box.Option (OptionsRow) as Box
import Blessed.UI.Forms.TextArea.Option (OptionsRow) as TextArea
import Blessed.UI.Forms.TextBox.Option (OptionsRow)
import Blessed.UI.Forms.TextBox.Event (TextBoxEvent)


type TextBox id state r = C.Node Subject.TextBox id ( Box.OptionsRow + TextArea.OptionsRow + OptionsRow + r ) state
type TextBoxAnd id state r = C.NodeAnd Subject.TextBox id ( Box.OptionsRow + TextArea.OptionsRow + OptionsRow + r ) state


textBox :: forall id r state. IsSymbol id => NodeKey Subject.TextBox id -> TextBox id state r
textBox nodeKey = C.node nodeKey


textBoxAnd :: forall id r state. IsSymbol id => NodeKey Subject.TextBox id -> TextBoxAnd id state r
textBoxAnd nodeKey = C.nodeAnd ( Proxy :: _ TextBoxEvent ) nodeKey
