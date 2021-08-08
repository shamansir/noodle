module App.Toolkit.UI where


import Prelude (Void, Unit)
import Effect.Class (class MonadEffect)


import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Node.Shape (InletId, OutletId)
import Noodle.Channel.Shape as Channel
import Noodle.Network (Network)

import App.Style.Color (Color)
import App.Style (Flags)

import Halogen as H


type BgInput d = Network d


type NodeInput d = Node d


data BgQuery a = BgCarry a


data NodeQuery a = NodeCarry a


type BgOutput d = Void


data NodeOutput d
    = SendToInlet InletId d
    | SendToOutlet OutletId d


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