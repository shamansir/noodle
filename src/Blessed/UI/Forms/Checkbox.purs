module Blessed.UI.Forms.Checkbox
    ( checkbox
    , checkboxAnd
    ) where


import Type.Row (type (+))
import Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(..))


import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.BlessedSubj (Checkbox) as Subject
import Blessed.Internal.NodeKey (NodeKey)
import Blessed.UI.Boxes.Box.Option (OptionsRow) as Box
import Blessed.UI.Forms.Checkbox.Option (OptionsRow)
import Blessed.UI.Forms.Checkbox.Event (CheckboxEvent)


type Checkbox id state r = C.Node Subject.Checkbox id ( Box.OptionsRow + OptionsRow + r ) state
type CheckboxAnd id state r = C.NodeAnd Subject.Checkbox id ( Box.OptionsRow + OptionsRow + r ) state


checkbox :: forall id r state. IsSymbol id => NodeKey Subject.Checkbox id -> Checkbox id state r
checkbox nodeKey = C.node nodeKey


checkboxAnd :: forall id r state. IsSymbol id => NodeKey Subject.Checkbox id -> CheckboxAnd id state r
checkboxAnd nodeKey = C.nodeAnd ( Proxy :: _ CheckboxEvent ) nodeKey