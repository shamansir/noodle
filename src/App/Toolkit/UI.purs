module App.Toolkit.UI where


import Prelude (Void, ($), (<$>))


import Data.Maybe (Maybe, fromMaybe)
import Data.Vec2 (Size)
import Data.Const (Const)

import Noodle.Node (Node)
import Noodle.Node as Node
import Noodle.Node.Shape (InletId, OutletId)
import Noodle.Channel.Shape as Channel
import Noodle.Network (Network)

import Color (Color)
import App.Style (Flags)
import App.Style (defaultFlags) as Style

import Halogen as H


type BgInput s d = { size :: Size, network :: Network d, state :: s }


type NodeInput s d = { node :: Node d, state :: s }


type PatchInput s d = { state :: s }


data BgQuery a = BgCarry a


data NodeQuery a = NodeCarry a


data PatchQuery a = PatchCarry a


type BgOutput s d = Void


data NodeOutput s d
    = SendToInlet InletId d
    | SendToOutlet OutletId d
    | Replace Node.Family
    | Update s


data PatchOutput state d
    = Update' state


type BgSlot s d id = H.Slot BgQuery (BgOutput s d) id


type NodeSlot s d id = H.Slot NodeQuery (NodeOutput s d) id


type BgComponent m s d = H.Component BgQuery (BgInput s d) (BgOutput s d) m


type NodeComponent m s d = H.Component NodeQuery (NodeInput s d) (NodeOutput s d) m


type PatchComponent m s d = H.Component PatchQuery (PatchInput s d) (PatchOutput s d) m


{- a.k.a. ToolkitUI -}
type UI m state d =
    { background :: Maybe (BgComponent m state d)
    , patch :: Maybe (PatchComponent m state d)
    , node :: Node.Family -> Maybe (NodeComponent m state d)
    , markNode :: Node.Family -> Maybe Color
    , markChannel :: Channel.Id -> Maybe Color
    , flags :: Node.Family -> Flags
    , state :: state
    }


flagsFor :: forall m s d. UI m s d -> Node d -> Flags
flagsFor ui node = fromMaybe Style.defaultFlags $ ui.flags <$> Node.family node