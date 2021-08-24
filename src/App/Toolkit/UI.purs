module App.Toolkit.UI where


import Prelude (Void, ($), (<$>))


import Data.Maybe (Maybe, fromMaybe)
import Data.Vec2 (Size)

import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Node.Shape (InletId, OutletId)
import Noodle.Channel.Shape as Channel
import Noodle.Network (Network)

import Data.Color (Color)
import App.Style (Flags)
import App.Style (defaultFlags) as Style

import Halogen as H


type BgInput d = { size :: Size, network :: Network d }


type NodeInput d = { node :: Node d }


data BgQuery a = BgCarry a


data NodeQuery a = NodeCarry a


type BgOutput :: forall k. k -> Type
type BgOutput d = Void


data NodeOutput d
    = SendToInlet InletId d
    | SendToOutlet OutletId d
    | Replace Node.Family


type BgSlot :: forall k. k -> Type -> Type
type BgSlot d id = H.Slot BgQuery (BgOutput d) id


type NodeSlot d id = H.Slot NodeQuery (NodeOutput d) id


type BgComponent m d = H.Component BgQuery (BgInput d) (BgOutput d) m


type NodeComponent m d = H.Component NodeQuery (NodeInput d) (NodeOutput d) m


type BgRenderer m d = Maybe (BgComponent m d)


type NodeRenderer m d = Node.Family -> Maybe (NodeComponent m d)


{- a.k.a. ToolkitUI -}
type UI m d =
    { background :: BgRenderer m d
    , node :: NodeRenderer m d
    , markNode :: Node.Family -> Maybe Color
    , markChannel :: Channel.Id -> Maybe Color
    , flags :: Node.Family -> Flags
    }


flagsFor :: forall m d. UI m d -> Node d -> Flags
flagsFor ui node = fromMaybe Style.defaultFlags $ ui.flags <$> Node.family node