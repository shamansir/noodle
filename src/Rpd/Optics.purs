module Rpd.Optics
    ( _entity, _pathToId, _cancelers
    , _networkPatches
    -- , _patch, _patchById, _patchNode, _patchNodes
    -- , _node, _nodes
    -- , _nodeInletsFlow, _nodeOutletsFlow, _nodeInletsPush
    -- , _nodeInlet, _nodeInlets
    -- , _nodeOutlet, _nodeOutlets
    -- , _nodeCancelers
    -- , _inlet, _inletLabel, _inletFlow, _inletPush, _inletCancelers
    -- , _outlet, _outletLabel, _outletFlow, _outletPush, _outletCancelers
    -- , _link, _linkCancelers
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



_patch :: forall d. Path.PatchPath -> Getter' (Network d) (Maybe (Patch d))
_patch = _pathGetter Path.ToPatch extractPatch


_patchById :: forall d. UUID.ToPatch -> Lens' (Network d) (Maybe (Patch d))
_patchById (UUID.ToPatch patchId) = _uuidLens PatchEntity extractPatch patchId


_node :: forall d. Path.NodePath -> Getter' (Network d) (Maybe (Node d))
_node = _pathGetter Path.ToNode extractNode


_nodeById :: forall d. UUID.ToNode -> Lens' (Network d) (Maybe (Node d))
_nodeById (UUID.ToNode nodeId) = _uuidLens NodeEntity extractNode nodeId


_inlet :: forall d. Path.InletPath -> Getter' (Network d) (Maybe (Inlet d))
_inlet = _pathGetter Path.ToInlet extractInlet


_inletById :: forall d. UUID.ToInlet -> Lens' (Network d) (Maybe (Inlet d))
_inletById (UUID.ToInlet inletId) = _uuidLens InletEntity extractInlet inletId


_outlet :: forall d. Path.OutletPath -> Getter' (Network d) (Maybe (Outlet d))
_outlet = _pathGetter Path.ToOutlet extractOutlet


_outletById :: forall d. UUID.ToOutlet -> Lens' (Network d) (Maybe (Outlet d))
_outletById (UUID.ToOutlet outletId) = _uuidLens OutletEntity extractOutlet outletId


_link :: forall d. Path.LinkPath -> Getter' (Network d) (Maybe Link)
_link = _pathGetter Path.ToLink extractLink


_linkById :: forall d. UUID.ToOutlet -> Lens' (Network d) (Maybe Link)
_linkById (UUID.ToOutlet linkId) = _uuidLens LinkEntity extractLink linkId


-- _networkPatch ::


_networkPatches :: forall d. Getter' (Network d) (List (Patch d))
_networkPatches =
    to \nw@(Network _ { patches }) ->
        (\uuid -> view (_patchById uuid) nw)
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
