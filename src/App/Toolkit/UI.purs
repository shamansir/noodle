module App.Toolkit.UI where


import Prelude (Unit, Void, ($), (<$>))


import Data.Maybe (Maybe, fromMaybe)
import Data.Vec2 (Size)
import Data.Const (Const)

import Noodle.Node (Node)
import Noodle.Patch (Patch)
import Noodle.Node as Node
import Noodle.Node.Shape (InletId, OutletId)
import Noodle.Channel.Shape as Channel
import Noodle.Network (Network)

import Color (Color)
import App.Style (Flags)
import App.Style (defaultFlags) as Style

import Halogen as H



{- type BgInput d = BgInput' Unit d
type BgInput' patch_state d = { size :: Size, network :: Network d, patchState :: patch_state } -}


type NodeInput d = NodeInput' Unit d
type NodeInput' patch_state d = { node :: Node d, patchState :: patch_state }


type PatchInput d = PatchInput' Unit d
type PatchInput' patch_state d = { size :: Size, patch :: Patch d, patchState :: patch_state }


--data BgQuery a = BgCarry a


data NodeQuery a = NodeCarry a


type PatchQuery a = PatchQuery' Unit a
type PatchQuery' patch_action a = TellPatch patch_action a


data TellPatch patch_action a =
    TellPatch patch_action a


type BgOutput = Void


type NodeOutput d = NodeOutput' Unit d
type NodeOutput' patch_action d = FromNode patch_action d


data FromNode patch_action d
    = SendToInlet InletId d
    | SendToOutlet OutletId d
    | Replace Node.Family
    | ToPatch patch_action


type PatchOutput = Void


type PatchSlot id = PatchSlot' Unit id
type PatchSlot' patch_action id = H.Slot (TellPatch patch_action) PatchOutput id


type NodeSlot patch_action d id = H.Slot NodeQuery (NodeOutput' patch_action d) id


{- type BgComponent d m = BgComponent' Unit d m
type BgComponent' patch_state d m = H.Component BgQuery (BgInput' patch_state d) BgOutput m -}


type NodeComponent d m = NodeComponent' Unit Unit d m
type NodeComponent' patch_action patch_state d m = H.Component NodeQuery (NodeInput' patch_state d) (NodeOutput' patch_action d) m


type PatchComponent d m = PatchComponent' Unit Unit d m
type PatchComponent' patch_action patch_state d m = H.Component (TellPatch patch_action) (PatchInput' patch_state d) PatchOutput m


type Components d m = Components' Unit Unit d m
type Components' patch_action patch_state d m =
    { patch :: Maybe (PatchComponent' patch_action patch_state d m)
    , node :: Node.Family -> Maybe (NodeComponent' patch_action patch_state d m)
    }


type Markings =
    { node :: Node.Family -> Maybe Color
    , channel :: Channel.Id -> Maybe Color
    }


type GetFlags = Node.Family -> Flags


flagsFor :: forall d. GetFlags -> Node d -> Flags
flagsFor getFlags node = fromMaybe Style.defaultFlags $ getFlags <$> Node.family node