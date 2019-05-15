module Rpd.Optics
    ( _entity, _pathToId, _cancelers
    , _networkPatches
    , _patch, _patchByPath
    , _node, _nodeByPath
    , _inlet, _inletByPath, _inletFlow, _inletPush
    , _outlet, _outletByPath, _outletFlow, _outletPush
    , _link, _linkByPath
    , _cancelers, _cancelersByPath
    , extractPatch, extractNode, extractInlet, extractOutlet, extractLink
    )
    where

import Prelude

import Data.Lens (Lens', Getter', lens, view, set, over, to)
import Data.Lens.At (at)

import Data.Maybe
import Data.List (List)
import Data.List as List
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\))

import Rpd.Network
import Rpd.Path (Path)
import Rpd.Path as Path
import Rpd.UUID (UUID)
import Rpd.UUID as UUID
import Rpd.Def
import Rpd.Util (Flow, PushableFlow(..), Canceler)


-- make separate lenses to access the entities registry by uuid,
-- then to read/write the first UUIDs from/to `pathToId`
-- and then combine/compose them to do everything else

_entity :: forall d. UUID -> Lens' (Network d) (Maybe (Entity d))
_entity uuid =
    lens getter setter
    where
        entityLens = at uuid
        getter (Network _ { registry }) = view entityLens registry
        setter (Network nwdef nwstate) val =
            Network
                nwdef
                nwstate { registry = set entityLens val nwstate.registry }


_pathToId :: forall d. Path -> Lens' (Network d) (Maybe UUID)
_pathToId path =
    lens getter setter
    where
        pathLens = at path
        getter (Network _ { pathToId }) =
            view pathLens pathToId
        setter (Network nwdef nwstate) val =
            Network
                nwdef
                nwstate { pathToId = set pathLens val nwstate.pathToId }


_pathGetter :: forall d x spath. (spath -> Path) -> (Entity d -> Maybe x) -> spath -> Getter' (Network d) (Maybe x)
_pathGetter adaptPath extractEntity spath =
    to \nw ->
        view (_pathToId $ adaptPath spath) nw
            >>= \uuid -> view (_entity uuid) nw
            >>= extractEntity


_uuidLens :: forall d x. (x -> Entity d) -> (Entity d -> Maybe x) -> UUID -> Lens' (Network d) (Maybe x)
_uuidLens adaptEntity extractEntity uuid =
    lens getter setter
    where
        getter nw =
            view (_entity uuid) nw
                >>= extractEntity
        setter nw@(Network nwdef nwstate) val =
            set (_entity uuid) (adaptEntity <$> val) nw


_patch :: forall d. UUID.ToPatch -> Lens' (Network d) (Maybe (Patch d))
_patch (UUID.ToPatch patchId) = _uuidLens PatchEntity extractPatch patchId


_patchByPath :: forall d. Path.PatchPath -> Getter' (Network d) (Maybe (Patch d))
_patchByPath = _pathGetter Path.ToPatch extractPatch


_node :: forall d. UUID.ToNode -> Lens' (Network d) (Maybe (Node d))
_node (UUID.ToNode nodeId) = _uuidLens NodeEntity extractNode nodeId

_nodeByPath :: forall d. Path.NodePath -> Getter' (Network d) (Maybe (Node d))
_nodeByPath = _pathGetter Path.ToNode extractNode


_inlet :: forall d. UUID.ToInlet -> Lens' (Network d) (Maybe (Inlet d))
_inlet (UUID.ToInlet inletId) = _uuidLens InletEntity extractInlet inletId


_inletByPath :: forall d. Path.InletPath -> Getter' (Network d) (Maybe (Inlet d))
_inletByPath = _pathGetter Path.ToInlet extractInlet


_outlet :: forall d. UUID.ToOutlet -> Lens' (Network d) (Maybe (Outlet d))
_outlet (UUID.ToOutlet outletId) = _uuidLens OutletEntity extractOutlet outletId


_outletByPath :: forall d. Path.OutletPath -> Getter' (Network d) (Maybe (Outlet d))
_outletByPath = _pathGetter Path.ToOutlet extractOutlet


_link :: forall d. UUID.ToOutlet -> Lens' (Network d) (Maybe Link)
_link (UUID.ToOutlet linkId) = _uuidLens LinkEntity extractLink linkId


_linkByPath :: forall d. Path.LinkPath -> Getter' (Network d) (Maybe Link)
_linkByPath = _pathGetter Path.ToLink extractLink


-- _networkPatch ::


_networkPatches :: forall d. Getter' (Network d) (List (Patch d))
_networkPatches =
    to \nw@(Network _ { patches }) ->
        (\uuid -> view (_patch uuid) nw)
            <$> List.fromFoldable patches
             #  List.catMaybes


_cancelers :: forall d. UUID -> Lens' (Network d) (Maybe (Array Canceler))
_cancelers uuid =
    lens getter setter
    where
        cancelersLens = at uuid
        getter (Network _ { cancelers }) =
            view cancelersLens cancelers
        setter (Network nwdef nwstate@{ cancelers }) val =
            Network
                nwdef
                nwstate
                    { cancelers = set cancelersLens val cancelers
                    }


_cancelersByPath :: forall d. Path -> Getter' (Network d) (Maybe (Array Canceler))
_cancelersByPath path =
    to \nw ->
        view (_pathToId path) nw
            >>= \uuid -> view (_cancelers uuid) nw


_inletFlow :: forall d. UUID.ToInlet -> Getter' (Network d) (Maybe (InletFlow d))
_inletFlow inletId =
    to extractFlow
    where
        inletLens = _inlet inletId
        extractFlow nw = view inletLens nw >>=
            \(Inlet _ _ _ { flow }) -> pure flow


_outletFlow :: forall d. UUID.ToOutlet -> Getter' (Network d) (Maybe (OutletFlow d))
_outletFlow outletId =
    to extractFlow
    where
        inletLens = _outlet outletId
        extractFlow nw = view inletLens nw >>=
            \(Outlet _ _ _ { flow }) -> pure flow


_inletPush :: forall d. UUID.ToInlet -> Getter' (Network d) (Maybe (PushToInlet d))
_inletPush inletId =
    to extractPFlow
    where
        inletLens = _inlet inletId
        extractPFlow nw = view inletLens nw >>=
            \(Inlet _ _ _ { push }) -> pure push


_outletPush :: forall d. UUID.ToOutlet -> Getter' (Network d) (Maybe (PushToOutlet d))
_outletPush outletId =
    to extractPFlow
    where
        outletLens = _outlet outletId
        extractPFlow nw = view outletLens nw >>=
            \(Outlet _ _ _ { push }) -> pure push


-- FIXME: These below are Prisms: https://github.com/purescript-contrib/purescript-profunctor-lenses/blob/master/examples/src/PrismsForSumTypes.purs


extractPatch :: forall d. Entity d -> Maybe (Patch d)
extractPatch (PatchEntity pEntity) = Just pEntity
extractPatch _ = Nothing


extractNode :: forall d. Entity d -> Maybe (Node d)
extractNode (NodeEntity nEntity) = Just nEntity
extractNode _ = Nothing


extractInlet :: forall d. Entity d -> Maybe (Inlet d)
extractInlet (InletEntity iEntity) = Just iEntity
extractInlet _ = Nothing


extractOutlet :: forall d. Entity d -> Maybe (Outlet d)
extractOutlet (OutletEntity oEntity) = Just oEntity
extractOutlet _ = Nothing


extractLink :: forall d. Entity d -> Maybe Link
extractLink (LinkEntity lEntity) = Just lEntity
extractLink _ = Nothing
