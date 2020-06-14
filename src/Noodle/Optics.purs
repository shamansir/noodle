module Noodle.Optics where

import Prelude

import Data.Lens (Lens', Getter', lens, view, set, over, to)
import Data.Lens.At (at)

import Data.Sequence as Seq
import Data.Sequence (Seq)

import Noodle.Network
import Noodle.Path as Path
import Noodle.Path (Path)
import Noodle.UUID (UUID)
import Noodle.UUID as UUID
import Noodle.Util (type (/->), Canceler)
import Noodle.Process (ProcessF)


_name :: forall d c n. Lens' (Network d c n) String
_name =
    lens getter setter
    where
        getter (Network { name }) = name
        setter (Network nwstate) val =
            Network nwstate { name = val }


_patches :: forall d c n. Lens' (Network d c n) (Seq UUID.ToPatch)
_patches =
    lens getter setter
    where
        getter (Network { patches }) = patches
        setter (Network nwstate) val =
            Network nwstate { patches = val }


_registry :: forall d c n. Lens' (Network d c n) (Seq UUID.ToPatch)
_registry =
    lens getter setter
    where
        getter (Network { patches }) = patches
        setter (Network nwstate) val =
            Network nwstate { patches = val }


_paths :: forall d c n. Lens' (Network d c n) (Path /-> UUID.Tagged)
_paths =
    lens getter setter
    where
        getter (Network { pathToId }) = pathToId
        setter (Network nwstate) val =
            Network nwstate { pathToId = val }


_cancelers :: forall d c n. Lens' (Network d c n) (UUID /-> Array Canceler)
_cancelers =
    lens getter setter
    where
        getter (Network { cancelers }) = cancelers
        setter (Network nwstate) val =
            Network nwstate { cancelers = val }


_patchId :: forall d c n. Lens' (Patch d c n) UUID.ToPatch
_patchId =
    lens getter setter
    where
        getter (Patch uuid _ _) = uuid
        setter (Patch _ path pstate) val =
            Patch val path pstate


_patchPath :: forall d c n. Lens' (Patch d c n) Path.ToPatch
_patchPath =
    lens getter setter
    where
        getter (Patch _ path _) = path
        setter (Patch uuid _ pstate) val =
            Patch uuid val pstate


_pathNodes :: forall d c n. Lens' (Patch d c n) (Seq UUID.ToNode)
_pathNodes =
    lens getter setter
    where
        getter (Patch _ _ { nodes }) = nodes
        setter (Patch uuid path pstate) val =
            Patch uuid path pstate { nodes = val }


_patchLinks :: forall d c n. Lens' (Patch d c n) (Seq UUID.ToLink)
_patchLinks =
    lens getter setter
    where
        getter (Patch _ _ { links }) = links
        setter (Patch uuid path pstate) val =
            Patch uuid path pstate { links = val }


_nodeId :: forall d n. Lens' (Node d n) UUID.ToNode
_nodeId =
    lens getter setter
    where
        getter (Node uuid _ _ _ _ _) = uuid
        setter (Node _ path n pos process nstate) val =
            Node val path n pos process nstate


_nodePath :: forall d n. Lens' (Node d n) Path.ToNode
_nodePath =
    lens getter setter
    where
        getter (Node _ path _ _ _ _) = path
        setter (Node uuid _ n pos process nstate) val =
            Node uuid val n pos process nstate


_nodeType :: forall d n. Lens' (Node d n) n
_nodeType =
    lens getter setter
    where
        getter (Node _ _ n _ _ _) = n
        setter (Node uuid path _ pos process nstate) val =
            Node uuid path val pos process nstate


_nodePosition :: forall d n. Lens' (Node d n) Position
_nodePosition =
    lens getter setter
    where
        getter (Node _ _ _ pos _ _) = pos
        setter (Node uuid path n _ process nstate) val =
            Node uuid path n val process nstate


_nodeProcess :: forall d n. Lens' (Node d n) (ProcessF d)
_nodeProcess =
    lens getter setter
    where
        getter (Node _ _ _ _ process _) = process
        setter (Node uuid path n pos process nstate) val =
            Node uuid path n pos val nstate


_nodeInlets :: forall d n. Lens' (Node d n) (Seq UUID.ToInlet)
_nodeInlets =
    lens getter setter
    where
        getter (Node _ _ _ _ _ { inlets }) = inlets
        setter (Node uuid path n pos process nstate) val =
            Node uuid path n pos process nstate { inlets = val }


_nodeOutlets :: forall d n. Lens' (Node d n) (Seq UUID.ToOutlet)
_nodeOutlets =
    lens getter setter
    where
        getter (Node _ _ _ _ _ { outlets }) = outlets
        setter (Node uuid path n pos process nstate) val =
            Node uuid path n pos process nstate { outlets = val }


_nodeInletsFlow :: forall d n. Lens' (Node d n) (InletsFlow d)
_nodeInletsFlow =
    lens getter setter
    where
        getter (Node _ _ _ _ _ { inletsFlow }) = inletsFlow
        setter (Node uuid path n pos process nstate) val =
            Node uuid path n pos process nstate { inletsFlow = val }


_nodeOutletsFlow :: forall d n. Lens' (Node d n) (OutletsFlow d)
_nodeOutletsFlow =
    lens getter setter
    where
        getter (Node _ _ _ _ _ { outletsFlow }) = outletsFlow
        setter (Node uuid path n pos process nstate) val =
            Node uuid path n pos process nstate { outletsFlow = val }


_nodePushToInlets :: forall d n. Lens' (Node d n) (PushToInlets d)
_nodePushToInlets =
    lens getter setter
    where
        getter (Node _ _ _ _ _ { pushToInlets }) = pushToInlets
        setter (Node uuid path n pos process nstate) val =
            Node uuid path n pos process nstate { pushToInlets = val }


_nodePushToOutlets :: forall d n. Lens' (Node d n) (PushToOutlets d)
_nodePushToOutlets =
    lens getter setter
    where
        getter (Node _ _ _ _ _ { pushToOutlets }) = pushToOutlets
        setter (Node uuid path n pos process nstate) val =
            Node uuid path n pos process nstate { pushToOutlets = val }


_inletId :: forall d c. Lens' (Inlet d c) UUID.ToInlet
_inletId =
    lens getter setter
    where
        getter (Inlet uuid _ _ _) = uuid
        setter (Inlet _ path c istate) val =
            Inlet val path c istate


_inletPath :: forall d c. Lens' (Inlet d c) Path.ToInlet
_inletPath =
    lens getter setter
    where
        getter (Inlet _ path _ _) = path
        setter (Inlet uuid _ c istate) val =
            Inlet uuid val c istate


_inletType :: forall d c. Lens' (Inlet d c) c
_inletType =
    lens getter setter
    where
        getter (Inlet _ _ c _) = c
        setter (Inlet uuid path _ istate) val =
            Inlet uuid path val istate


_inletFlow :: forall d c. Lens' (Inlet d c) (InletFlow d)
_inletFlow =
    lens getter setter
    where
        getter (Inlet _ _ _ { flow }) = flow
        setter (Inlet uuid path c istate) val =
            Inlet uuid path c istate { flow = val }


_inletPush :: forall d c. Lens' (Inlet d c) (PushToInlet d)
_inletPush =
    lens getter setter
    where
        getter (Inlet _ _ _ { push }) = push
        setter (Inlet uuid path c istate) val =
            Inlet uuid path c istate { push = val }


_outletId :: forall d c. Lens' (Inlet d c) UUID.ToInlet
_outletId =
    lens getter setter
    where
        getter (Inlet uuid _ _ _) = uuid
        setter (Inlet _ path c istate) val =
            Inlet val path c istate


_outletPath :: forall d c. Lens' (Outlet d c) Path.ToOutlet
_outletPath =
    lens getter setter
    where
        getter (Outlet _ path _ _) = path
        setter (Outlet uuid _ c ostate) val =
            Outlet uuid val c ostate


_outletType :: forall d c. Lens' (Outlet d c) c
_outletType =
    lens getter setter
    where
        getter (Outlet _ _ c _) = c
        setter (Outlet uuid path _ ostate) val =
            Outlet uuid path val ostate


_outletFlow :: forall d c. Lens' (Outlet d c) (OutletFlow d)
_outletFlow =
    lens getter setter
    where
        getter (Outlet _ _ _ { flow }) = flow
        setter (Outlet uuid path c ostate) val =
            Outlet uuid path c ostate { flow = val }


_outletPush :: forall d c. Lens' (Outlet d c) (PushToOutlet d)
_outletPush =
    lens getter setter
    where
        getter (Outlet _ _ _ { push }) = push
        setter (Outlet uuid path c ostate) val =
            Outlet uuid path c ostate { push = val }


_linkId :: Lens' Link UUID.ToLink
_linkId =
    lens getter setter
    where
        getter (Link uuid _) = uuid
        setter (Link _ lstate) val =
            Link val lstate


_linkOutletId :: Lens' Link UUID.ToOutlet
_linkOutletId =
    lens getter setter
    where
        getter (Link _ { outlet }) = outlet
        setter (Link uuid lstate) val =
            Link uuid lstate { outlet = val }


_linkInletId :: Lens' Link UUID.ToInlet
_linkInletId =
    lens getter setter
    where
        getter (Link _ { inlet }) = inlet
        setter (Link uuid lstate) val =
            Link uuid lstate { inlet = val }
