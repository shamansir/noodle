module App.Toolkit.UI where


import Prelude (Unit, Void, ($), (<$>), (<<<))


import Data.Maybe (Maybe, fromMaybe)
import Data.Vec2 (Size)
import Data.Const (Const)

import Noodle.Node (Node)
import Noodle.Patch (Patch)
import Noodle.Node as Node
import Noodle.Network (Network)
import Noodle.Channel as Channel

import Color (Color)
import App.Style (Flags)
import App.Style (defaultFlags) as Style

import Effect.Aff (Aff)

import Halogen as H



{- type BgInput d = BgInput' Unit d
type BgInput' patch_state d = { size :: Size, network :: Network d, patchState :: patch_state } -}


{- Node -}


-- FIXME: get rid of `patch_action` -> with PatchM it seems to not be needed
-- FIXME: patch_state == node_state ?


type NodeSlot patch_action d id = H.Slot NodeQuery (NodeOutput' patch_action d) id


type NodeInput d = NodeInput' Unit Unit d
type NodeInput' patch_state node_state d = { node :: Node node_state d, patchState :: patch_state }


type NodeOutput d = NodeOutput' Void d
type NodeOutput' patch_action d = FromNode patch_action d


data NodeQuery a = NodeCarry a


data FromNode patch_action d
    = SendToInlet Node.InletId d
    | SendToOutlet Node.OutletId d
    | Replace Node.Family
    | ToPatch patch_action


type NodeComponent d = NodeComponent' Void Unit Unit d
type NodeComponent' patch_action patch_state node_state d =
    H.Component NodeQuery (NodeInput' patch_state node_state d) (NodeOutput' patch_action d) Aff


{- Patch -}


type PatchSlot id = PatchSlot' Void Unit id
type PatchSlot' patch_action patch_state id = H.Slot (TellPatch patch_action) (InformApp patch_state) id


type PatchInput d = PatchInput' Unit d
type PatchInput' patch_state d = { size :: Size, patch :: Patch patch_state d, patchState :: patch_state }


type PatchOutput = PatchOutput' Unit
type PatchOutput' patch_state = InformApp patch_state


type PatchQuery a = PatchQuery' Void a
type PatchQuery' patch_action a = TellPatch patch_action a


data InformApp patch_state
    = Next patch_state


data TellPatch patch_action a
    = TellPatch patch_action a


type PatchComponent d = PatchComponent' Void Unit d
type PatchComponent' patch_action patch_state d =
    H.Component (TellPatch patch_action) (PatchInput' patch_state d) (PatchOutput' patch_state) Aff


{- Components -}


type Components d = Components' Void Unit Unit d
type Components' patch_action patch_state node_state d =
    { patch :: Maybe (PatchComponent' patch_action patch_state d)
    , node :: Node.Family -> Maybe (NodeComponent' patch_action patch_state node_state d)
    }


{- Markings -}


type Markings =
    { node :: Node.Family -> Maybe Color
    , channel :: Channel.Id -> Maybe Color
    }


{- Flags -}


type GetFlags = Node.Family -> Flags


flagsFor :: forall state d. GetFlags -> Node state d -> Flags
flagsFor getFlags = getFlags <<< Node.family