module Noodle.Optics where

import Prelude

import Data.Lens
import Data.Lens.At (at)
import Data.Lens.Prism.Maybe (_Just)

import Data.Maybe (Maybe(..))
import Data.Sequence (Seq)
import Data.Sequence.Extra as Seq

import Noodle.Network
import Noodle.Path as Path
import Noodle.Path (Path)
import Noodle.UUID (UUID)
import Noodle.UUID as UUID
import Noodle.Util (type (/->), Canceler)
import Noodle.Process (ProcessF)

type MLens' s a = Lens' s (Maybe a)
type MGetter' s a = Getter' s (Maybe a)


{- REGISTRY -}


_registry :: forall d c n. Lens' (Network d c n) (UUID.Tagged /-> Entity d c n)
_registry =
    lens getter setter
    where
        getter (Network { registry }) = registry
        setter (Network nwstate) val =
            Network nwstate { registry = val }


_entity :: forall d c n. UUID.Tagged -> MLens' (Network d c n) (Entity d c n)
_entity uuid = _registry <<< at uuid


_patch :: forall d c n. UUID.ToPatch -> Traversal' (Network d c n) (Patch d c n)
_patch uuid = _entity (UUID.liftTagged uuid) <<< _Just <<< _entityToPatch


_node :: forall d c n. UUID.ToNode -> Traversal' (Network d c n) (Node d n)
_node uuid = _entity (UUID.liftTagged uuid) <<< _Just <<< _entityToNode


_inlet :: forall d c n. UUID.ToInlet -> Traversal' (Network d c n) (Inlet d c)
_inlet uuid = _entity (UUID.liftTagged uuid) <<< _Just <<< _entityToInlet


_outlet :: forall d c n. UUID.ToOutlet -> Traversal' (Network d c n) (Outlet d c)
_outlet uuid = _entity (UUID.liftTagged uuid) <<< _Just <<< _entityToOutlet


_link :: forall d c n. UUID.ToLink -> Traversal' (Network d c n) Link
_link uuid = _entity (UUID.liftTagged uuid) <<< _Just <<< _entityToLink


_entityByPath :: forall d c n. Path -> MLens' (Network d c n) (Entity d c n)
_entityByPath path =
    lens getter setter
    where
        getter nw =
            view (_pathToId path) nw >>=
                \uuid -> view (_entity uuid) nw
        setter nw val =
            case view (_pathToId path) nw of
                Just uuid -> set (_entity uuid) val nw
                Nothing -> nw


_patchByPath :: forall d c n. Path.ToPatch -> Traversal' (Network d c n) (Patch d c n)
_patchByPath path = _entityByPath (Path.lift path) <<< _Just <<< _entityToPatch


_nodeByPath :: forall d c n. Path.ToNode -> Traversal' (Network d c n) (Node d n)
_nodeByPath path = _entityByPath (Path.lift path) <<< _Just <<< _entityToNode


_inletByPath :: forall d c n. Path.ToInlet -> Traversal' (Network d c n) (Inlet d c)
_inletByPath path = _entityByPath (Path.lift path) <<< _Just <<< _entityToInlet


_outletByPath :: forall d c n. Path.ToOutlet -> Traversal' (Network d c n) (Outlet d c)
_outletByPath path = _entityByPath (Path.lift path) <<< _Just <<< _entityToOutlet


{- NETWORK -}


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


-- _linkByPath :: forall d c n. Path.ToLink -> Traversal' (Network d c n) Link
-- _linkByPath path = _entityByPath (Path.lift path) <<< _Just <<< _entityToLink


_paths :: forall d c n. Lens' (Network d c n) (Path /-> UUID.Tagged)
_paths =
    lens getter setter
    where
        getter (Network { pathToId }) = pathToId
        setter (Network nwstate) val =
            Network nwstate { pathToId = val }


_pathToId :: forall d c n. Path -> MLens' (Network d c n) UUID.Tagged
_pathToId path =
    _paths <<< at path


_cancelers :: forall d c n. Lens' (Network d c n) (UUID /-> Array Canceler)
_cancelers =
    lens getter setter
    where
        getter (Network { cancelers }) = cancelers
        setter (Network nwstate) val =
            Network nwstate { cancelers = val }


{- PATCH -}


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


_patchNodes :: forall d c n. Lens' (Patch d c n) (Seq UUID.ToNode)
_patchNodes =
    lens getter setter
    where
        getter (Patch _ _ { nodes }) = nodes
        setter (Patch uuid path pstate) val =
            Patch uuid path pstate { nodes = val }


-- _patchNode :: forall d c n. UUID.ToNode -> MLens' (Patch d c n) Unit
-- _patchNode n = _patchNodes <<< Seq._on n


-- _patchNodesByPath :: forall d c n. Path.ToPatch -> Lens' (Patch d c n) (Seq UUID.ToNode)
-- _patchNodesByPath p = _patchByPath p <<< _patchNode


_patchLinks :: forall d c n. Lens' (Patch d c n) (Seq UUID.ToLink)
_patchLinks =
    lens getter setter
    where
        getter (Patch _ _ { links }) = links
        setter (Patch uuid path pstate) val =
            Patch uuid path pstate { links = val }


-- _patchLink :: forall d c n. UUID.ToLink -> MLens' (Patch d c n) Unit
-- _patchLink l = _patchLinks <<< Seq._on l


{- NODE -}


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


-- _nodeInlet :: forall d n. UUID.ToInlet -> MLens' (Node d n) Unit
-- _nodeInlet i = _nodeInlets <<< Seq._on i


_nodeOutlets :: forall d n. Lens' (Node d n) (Seq UUID.ToOutlet)
_nodeOutlets =
    lens getter setter
    where
        getter (Node _ _ _ _ _ { outlets }) = outlets
        setter (Node uuid path n pos process nstate) val =
            Node uuid path n pos process nstate { outlets = val }


-- _nodeOutlet :: forall d n. UUID.ToOutlet -> MLens' (Node d n) Unit
-- _nodeOutlet o = _nodeOutlets <<< Seq._on o


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


{- INLET -}


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


{- OUTLET -}


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


{- LINK -}


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


{- ENTITY -}


_entityToPatch :: forall d c n. Prism' (Entity d c n) (Patch d c n)
_entityToPatch =
    prism' PatchEntity extractPatch
    where
        extractPatch (PatchEntity patch) = Just patch
        extractPatch _ = Nothing


_entityToNode :: forall d c n. Prism' (Entity d c n) (Node d n)
_entityToNode =
    prism' NodeEntity extractNode
    where
        extractNode (NodeEntity node) = Just node
        extractNode _ = Nothing


_entityToInlet :: forall d c n. Prism' (Entity d c n) (Inlet d c)
_entityToInlet =
    prism' InletEntity extractInlet
    where
        extractInlet (InletEntity inlet) = Just inlet
        extractInlet _ = Nothing


_entityToOutlet :: forall d c n. Prism' (Entity d c n) (Outlet d c)
_entityToOutlet =
    prism' OutletEntity extractOutlet
    where
        extractOutlet (OutletEntity outlet) = Just outlet
        extractOutlet _ = Nothing


_entityToLink :: forall d c n. Prism' (Entity d c n) Link
_entityToLink =
    prism' LinkEntity extractLink
    where
        extractLink (LinkEntity link) = Just link
        extractLink _ = Nothing
