module Rpd.Optics
    ( _patch, _patches, _patchNode, _patchNodes
    , _node, _nodes
    , _nodeFlow, _nodePFlow
    , _nodeInlet, _nodeInlets
    , _nodeOutlet, _nodeOutlets
    , _nodeCanceler
    , _inlet, _inletLabel, _inletFlow, _inletPFlow, _inletCanceler
    , _outlet, _outletLabel, _outletFlow, _outletPFlow
    , _link, _linkCanceler
    )
    where

import Data.Maybe
import Prelude

import Data.Lens (Lens', Getter', lens, view, set, over, to)
import Data.Lens.At (at)
import Data.List (List)
import Data.List as List
import Data.Tuple.Nested (type (/\))

import Rpd.Network
import Rpd.Path
import Rpd.Def
import Rpd.Util (Flow, PushableFlow(..), Canceler)


_patch :: forall d. PatchId -> Lens' (Network d) (Maybe (Patch d))
_patch patchId =
    lens getter setter
    where
        patchLens = at patchId
        getter (Network _ { patches }) = view patchLens patches
        setter (Network nwdef nwstate) val =
            Network
                nwdef
                nwstate { patches = set patchLens val nwstate.patches }


_patches :: forall d. Getter' (Network d) (List (Patch d))
_patches =
    to \(Network _ { patches }) -> List.fromFoldable patches


_patchNode :: forall d. PatchId -> NodePath -> Lens' (Network d) (Maybe Unit)
_patchNode patchId nodePath =
    lens getter setter
    where
        patchLens = _patch patchId
        nodeLens = at nodePath
        getter nw =
            view patchLens nw
            >>= \(Patch _ _ { nodes }) -> view nodeLens nodes
        setter nw val =
            over patchLens
                (map $ \(Patch pid pdef pstate) ->
                    Patch
                        pid
                        pdef
                        pstate { nodes = set nodeLens val pstate.nodes }
                ) nw


_patchNodes :: forall d. PatchId -> Getter' (Network d) (List (Node d))
_patchNodes patchId =
    to extractNodes
    where
        patchLens = _patch patchId
        getNodePaths nw =
            view patchLens nw >>=
                \(Patch _ _ { nodes }) ->
                    pure $ List.fromFoldable nodes
        getNode nodePath = view (_node nodePath)
        extractNodes nw@(Network _ { nodes }) =
            let nodePaths = fromMaybe List.Nil $ getNodePaths nw
            in map (flip getNode $ nw) nodePaths # List.catMaybes


_node :: forall d. NodePath -> Lens' (Network d) (Maybe (Node d))
_node nodePath@(NodePath patchId _) =
    lens getter setter
    where
        nodeLens = at nodePath
        getter (Network _ { nodes }) = view nodeLens nodes
        setter (Network nwdef nwstate) val =
            Network
                nwdef
                nwstate { nodes = set nodeLens val nwstate.nodes }


_nodes :: forall d. Getter' (Network d) (List (Node d))
_nodes =
    to \(Network _ { nodes }) -> List.fromFoldable nodes


_nodeFlow :: forall d. NodePath -> Getter' (Network d) (Maybe (Flow (Int /\ d)))
_nodeFlow nodePath =
    to extractFlow
    where
        pFlowLens = _nodePFlow nodePath
        extractFlow nw = view pFlowLens nw >>=
            \(PushableFlow _ flow) -> pure flow


_nodePFlow :: forall d. NodePath -> Getter' (Network d) (Maybe (PushableFlow (Int /\ d)))
_nodePFlow nodePath =
    to extractPFlow
    where
        nodeLens = _node nodePath
        extractPFlow nw = view nodeLens nw >>=
            \(Node _ _ { flow }) ->
                case flow of
                    (ProcessPFlow pFlow) -> pure pFlow


_nodeInlet :: forall d. NodePath -> InletPath -> Lens' (Network d) (Maybe Unit)
_nodeInlet nodePath inletPath =
    lens getter setter
    where
        nodeLens = _node nodePath
        inletLens = at inletPath
        getter nw =
            view nodeLens nw
            >>= \(Node _ _ { inlets }) -> view inletLens inlets
        setter nw val =
            over nodeLens
                (map $ \(Node nid ndef nstate) ->
                    Node
                        nid
                        ndef
                        nstate { inlets = set inletLens val nstate.inlets }
                ) nw


_nodeInlets :: forall d. NodePath -> Getter' (Network d) (List (Inlet d))
_nodeInlets nodePath =
    to extractInlets
    where
        nodeLens = _node nodePath
        getNodeInlets nw =
            view nodeLens nw >>=
                \(Node _ _ { inlets }) ->
                    pure $ List.fromFoldable inlets
        getInlet inletPath = view (_inlet inletPath)
        extractInlets nw@(Network _ { inlets }) =
            let inletPaths = fromMaybe List.Nil $ getNodeInlets nw
            in map (flip getInlet $ nw) inletPaths # List.catMaybes


_nodeOutlet :: forall d. NodePath -> OutletPath -> Lens' (Network d) (Maybe Unit)
_nodeOutlet nodePath outletPath =
    lens getter setter
    where
        nodeLens = _node nodePath
        outletLens = at outletPath
        getter nw =
            view nodeLens nw
            >>= \(Node _ _ { outlets }) -> view outletLens outlets
        setter nw val =
            over nodeLens
                (map $ \(Node nid ndef nstate) ->
                    Node
                        nid
                        ndef
                        nstate { outlets = set outletLens val nstate.outlets }
                ) nw


_nodeOutlets :: forall d. NodePath -> Getter' (Network d) (List (Outlet d))
_nodeOutlets nodePath =
    to extractOutlets
    where
        nodeLens = _node nodePath
        getNodeOutlets nw =
            view nodeLens nw >>=
                \(Node _ _ { outlets }) ->
                    pure $ List.fromFoldable outlets
        getOutlet outletPath = view (_outlet outletPath)
        extractOutlets nw =
            let outletPaths = fromMaybe List.Nil $ getNodeOutlets nw
            in map (flip getOutlet $ nw) outletPaths # List.catMaybes


_nodeCanceler :: forall d. NodePath -> Lens' (Network d) (Maybe Canceler)
_nodeCanceler nodePath =
    lens getter setter
    where
        cancelerLens = at nodePath
        getter (Network _ { cancelers }) =
            view cancelerLens cancelers.nodes
        setter (Network nwdef nwstate@{ cancelers }) val =
            Network
                nwdef
                nwstate {
                    cancelers =
                        cancelers { nodes = set cancelerLens val cancelers.nodes }
                    }


_inlet :: forall d. InletPath -> Lens' (Network d) (Maybe (Inlet d))
_inlet inletPath@(InletPath nodePath _) =
    lens getter setter
    where
        inletLens = at inletPath
        getter (Network _ { inlets }) = view inletLens inlets
        setter (Network nwdef nwstate) val =
            Network
                nwdef
                nwstate { inlets = set inletLens val nwstate.inlets }
            -- # set (_nodeInlet nodePath inletPath) (const unit <$> val)


_inletLabel :: forall d. InletPath -> Getter' (Network d) (Maybe String)
_inletLabel inletPath =
    to extractLabel
    where
        inletLens = _inlet inletPath
        extractLabel nw = view inletLens nw >>=
            \(Inlet _ { label } _) -> pure label


_inletFlow :: forall d. InletPath -> Getter' (Network d) (Maybe (Flow d))
_inletFlow inletPath =
    to extractFlow
    where
        pFlowLens = _inletPFlow inletPath
        extractFlow nw = view pFlowLens nw >>=
            \(PushableFlow _ flow) -> pure flow


_inletPFlow :: forall d. InletPath -> Getter' (Network d) (Maybe (PushableFlow d))
_inletPFlow inletPath =
    to extractPFlow
    where
        inletLens = _inlet inletPath
        extractPFlow nw = view inletLens nw >>=
            \(Inlet _ _ { flow }) ->
                case flow of
                    (InletPFlow pFlow) -> pure pFlow


_inletCanceler :: forall d. InletPath -> Lens' (Network d) (Maybe Canceler)
_inletCanceler inletPath =
    lens getter setter
    where
        cancelerLens = at inletPath
        getter (Network _ { cancelers }) =
            view cancelerLens cancelers.inlets
        setter (Network nwdef nwstate@{ cancelers }) val =
            Network
                nwdef
                nwstate {
                    cancelers =
                        cancelers { inlets = set cancelerLens val cancelers.inlets }
                    }


_outlet :: forall d. OutletPath -> Lens' (Network d) (Maybe (Outlet d))
_outlet outletPath@(OutletPath nodePath _) =
    lens getter setter
    where
        outletLens = at outletPath
        getter (Network _ { outlets }) = view outletLens outlets
        setter (Network nwdef nwstate) val =
            Network
                nwdef
                nwstate { outlets = set outletLens val nwstate.outlets }
            -- # set (_nodeOutlet nodePath outletPath) (const unit <$> val)


_outletLabel :: forall d. OutletPath -> Getter' (Network d) (Maybe String)
_outletLabel outletPath =
    to extractLabel
    where
        outletLens = _outlet outletPath
        extractLabel nw = view outletLens nw >>=
            \(Outlet _ { label } _) -> pure label


_outletFlow :: forall d. OutletPath -> Getter' (Network d) (Maybe (Flow d))
_outletFlow outletPath =
    to extractFlow
    where
        pFlowLens = _outletPFlow outletPath
        extractFlow nw = view pFlowLens nw >>=
            \(PushableFlow _ flow) -> pure flow


_outletPFlow :: forall d. OutletPath -> Getter' (Network d) (Maybe (PushableFlow d))
_outletPFlow outletPath =
    to extractPFlow
    where
        outletLens = _outlet outletPath
        extractPFlow nw =
            view outletLens nw >>=
                \(Outlet _ _ { flow }) ->
                    case flow of
                        (OutletPFlow pFlow) -> pure pFlow


_link :: forall d. LinkId -> Lens' (Network d) (Maybe Link)
_link linkId =
    lens getter setter
    where
        linkLens = at linkId
        getter (Network _ { links }) = view linkLens links
        setter nw@(Network nwdef nwstate) val =
            Network
                nwdef
                nwstate { links = set linkLens val nwstate.links }


_linkCanceler :: forall d. LinkId -> Lens' (Network d) (Maybe Canceler)
_linkCanceler linkId =
    lens getter setter
    where
        cancelerLens = at linkId
        getter (Network _ { cancelers }) =
            view cancelerLens cancelers.links
        setter (Network nwdef nwstate@{ cancelers }) val =
            Network
                nwdef
                nwstate {
                    cancelers =
                        cancelers { links = set cancelerLens val cancelers.links }
                    }
