module Blessed.UI.Boxes.Box
    ( box
    , boxAnd
    ) where


import Type.Row (type (+))
import Data.Symbol (class IsSymbol)

import Blessed.UI.Boxes.Box.Option (OptionsRow)
import Blessed.UI.Boxes.Box.Event (Event)

import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.BlessedSubj (Box) as Subject
import Blessed.Internal.NodeKey (NodeKey)


box :: forall id r state. IsSymbol id => NodeKey Subject.Box id -> C.Node Subject.Box id ( OptionsRow + r ) state Event
box nodeKey = C.node nodeKey


boxAnd :: forall id r state. IsSymbol id => NodeKey Subject.Box id -> C.NodeAnd Subject.Box id ( OptionsRow + r ) state Event
boxAnd nodeKey = C.nodeAnd nodeKey