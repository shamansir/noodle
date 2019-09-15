module Rpd.Optics
    ( _entity, _pathToId
    , _networkPatch
    , _networkPatches, _networkNodes, _networkInlets, _networkOutlets, _networkLinks
    , _patch, _patchByPath, _patchNode, _patchLink
    , _patchNodes, _patchNodesByPath, _patchLinks, _patchLinksByPath
    , _node, _nodeByPath, _nodeInlet, _nodeOutlet, _nodeInletsFlow, _nodeOutletsFlow
    , _nodeInlets, _nodeInletsByPath, _nodeOutlets, _nodeOutletsByPath
    , _inlet, _inletByPath, _inletFlow, _inletPush
    , _outlet, _outletByPath, _outletFlow, _outletPush
    , _link--, _linkByPath
    , _cancelers, _cancelersByPath
    , extractPatch, extractNode, extractInlet, extractOutlet, extractLink
    )
    where

import Prelude

import Data.Lens (Lens', Getter', lens, view, set, over, to)
import Data.Lens.At (at)

import Data.Maybe (Maybe(..))
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Tuple (fst) as Tuple
import Data.Tuple.Nested (type (/\))

import Rpd.Network
import Rpd.Path (Path)
import Rpd.Path as Path
import Rpd.UUID (UUID)
import Rpd.UUID as UUID
import Rpd.Util (Flow, PushableFlow(..), Canceler)
import Rpd.Util (seqMember', seqDelete, seqCatMaybes) as Util


-- make separate lenses to access the entities registry by uuid,
-- then to read/write the first UUIDs from/to `pathToId`
-- and then combine/compose them to do everything else

_entity :: forall d c n. UUID.Tagged -> Lens' (Network d c n) (Maybe (Entity d c n))
_entity uuid =
    lens getter setter
    where
        entityLens = at uuid
        getter (Network { registry }) = view entityLens registry
        setter (Network nwstate) val =
            Network
                nwstate { registry = set entityLens val nwstate.registry }


_pathToId :: forall d c n. Path -> Lens' (Network d c n) (Maybe UUID.Tagged)
_pathToId path =
    lens getter setter
    where
        pathLens = at path
        getter (Network { pathToId }) =
            view pathLens pathToId
        setter (Network nwstate) val =
            Network
                nwstate { pathToId = set pathLens val nwstate.pathToId }


_pathGetter
    :: forall d c n x
     . (Entity d c n -> Maybe x)
    -> Path
    -> Getter' (Network d c n) (Maybe x)
_pathGetter extractEntity path =
    to \nw ->
        view (_pathToId path) nw
            >>= \uuid -> view (_entity uuid) nw
            >>= extractEntity


_uuidLens
    :: forall d c n x
     . (x -> Entity d c n)
    -> (Entity d c n -> Maybe x)
    -> UUID.Tagged
    -> Lens' (Network d c n) (Maybe x)
_uuidLens adaptEntity extractEntity uuid =
    lens getter setter
    where
        getter nw =
            view (_entity uuid) nw
                >>= extractEntity
        setter nw@(Network nwstate) val =
            set (_entity uuid) (adaptEntity <$> val) nw


_seq :: forall a. Eq a => a -> Lens' (Seq a) (Maybe Unit)
_seq v =
    lens getter setter
    where
        getter = Util.seqMember' v
        setter seq maybeVal =
            case maybeVal of
                Just val -> v # Seq.snoc seq
                Nothing -> seq # Util.seqDelete v


_patch :: forall d c n. UUID.ToPatch -> Lens' (Network d c n) (Maybe (Patch d c n))
_patch uuid = _uuidLens PatchEntity extractPatch $ UUID.liftTagged uuid


_patchByPath :: forall d c n. Path.ToPatch -> Getter' (Network d c n) (Maybe (Patch d c n))
_patchByPath = Path.lift >>> _pathGetter extractPatch


_patchNode :: forall d c n. UUID.ToPatch -> UUID.ToNode -> Lens' (Network d c n) (Maybe Unit)
_patchNode patchUuid nodeUuid =
    lens getter setter
    where
        patchLens = _patch patchUuid
        nodeLens = _seq nodeUuid
        getter nw =
            view patchLens nw
                >>= \(Patch _ _ { nodes }) -> view nodeLens nodes
        setter nw val =
            over patchLens
                (map $ \(Patch puuid ppath pstate@{ nodes }) ->
                    Patch
                        puuid
                        ppath
                        (pstate
                            { nodes = set nodeLens val nodes
                            })
                ) nw


_patchNodes :: forall d c n. UUID.ToPatch -> Getter' (Network d c n) (Maybe (Seq (Node d n)))
_patchNodes patchUuid =
    to \nw ->
        view (_patch patchUuid) nw
            >>= \(Patch _ _ { nodes }) ->
                pure $ viewNode nw <$> nodes # Util.seqCatMaybes
    where
        viewNode nw nodeUiid = view (_node nodeUiid) nw


_patchNodesByPath :: forall d c n. Path.ToPatch -> Getter' (Network d c n) (Maybe (Seq (Node d n)))
_patchNodesByPath patchUuid =
    to \nw ->
        view (_pathToId $ Path.lift patchUuid) nw
            >>= UUID.toPatch
            >>= \uuid -> view (_patchNodes uuid) nw


_patchLink :: forall d c n. UUID.ToPatch -> UUID.ToLink -> Lens' (Network d c n) (Maybe Unit)
_patchLink patchUuid linkUuid =
    lens getter setter
    where
        patchLens = _patch patchUuid
        linkLens = _seq linkUuid
        getter nw =
            view patchLens nw
                >>= \(Patch _ _ { links }) -> view linkLens links
        setter nw val =
            over patchLens
                (map $ \(Patch puuid ppath pstate@{ links }) ->
                    Patch
                        puuid
                        ppath
                        (pstate
                            { links = set linkLens val links
                            })
                ) nw


_patchLinks :: forall d c n. UUID.ToPatch -> Getter' (Network d c n) (Maybe (Seq Link))
_patchLinks patchUuid =
    to \nw ->
        view (_patch patchUuid) nw
            >>= \(Patch _ _ { links }) ->
                pure $ viewLink nw <$> links # Util.seqCatMaybes
    where
        viewLink nw linkUuid = view (_link linkUuid) nw


_patchLinksByPath :: forall d c n. Path.ToPatch -> Getter' (Network d c n) (Maybe (Seq Link))
_patchLinksByPath patchUuid =
    to \nw ->
        view (_pathToId $ Path.lift patchUuid) nw
            >>= UUID.toPatch
            >>= \uuid -> view (_patchLinks uuid) nw



_node :: forall d c n. UUID.ToNode -> Lens' (Network d c n) (Maybe (Node d n))
_node uuid = _uuidLens NodeEntity extractNode $ UUID.liftTagged uuid


_nodeByPath :: forall d c n. Path.ToNode -> Getter' (Network d c n) (Maybe (Node d n))
_nodeByPath = Path.lift >>> _pathGetter extractNode


_nodeInletsFlow :: forall d c n. UUID.ToNode -> Getter' (Network d c n) (Maybe (InletsFlow d))
_nodeInletsFlow nodeUuid =
    to \nw ->
        view (_node nodeUuid) nw
            >>= \(Node _ _ _ _ { inletsFlow }) -> pure inletsFlow


_nodeOutletsFlow :: forall d c n. UUID.ToNode -> Getter' (Network d c n) (Maybe (OutletsFlow d))
_nodeOutletsFlow nodeUuid =
    to \nw ->
        view (_node nodeUuid) nw
            >>= \(Node _ _ _ _ { outletsFlow }) -> pure outletsFlow


_nodeInlet :: forall d c n. UUID.ToNode -> UUID.ToInlet -> Lens' (Network d c n) (Maybe Unit)
_nodeInlet nodeUuid inletUuid =
    lens getter setter
    where
        nodeLens = _node nodeUuid
        inletLens = _seq inletUuid
        getter nw =
            view nodeLens nw
                >>= \(Node _ _ _ _ { inlets }) -> view inletLens inlets
        setter nw val =
            over nodeLens
                (map $ \(Node nuuid npath n process nstate@{ inlets }) ->
                    Node
                        nuuid
                        npath
                        n
                        process
                        nstate { inlets = set inletLens val inlets }
                ) nw


_nodeInlets :: forall d c n. UUID.ToNode -> Getter' (Network d c n) (Maybe (Seq (Inlet d c)))
_nodeInlets nodeUuid =
    to \nw ->
        view (_node nodeUuid) nw
            >>= \(Node _ _ _ _ { inlets }) ->
                pure $ viewInlet nw <$> inlets # Util.seqCatMaybes
    where
        viewInlet nw inletUiid = view (_inlet inletUiid) nw


_nodeInletsByPath :: forall d c n. Path.ToNode -> Getter' (Network d c n) (Maybe (Seq (Inlet d c)))
_nodeInletsByPath nodePath =
    to \nw ->
        view (_pathToId $ Path.lift nodePath) nw
            >>= UUID.toNode
            >>= \uuid -> view (_nodeInlets uuid) nw


_nodeOutlet :: forall d c n. UUID.ToNode -> UUID.ToOutlet -> Lens' (Network d c n) (Maybe Unit)
_nodeOutlet nodeUuid outletUuid =
    lens getter setter
    where
        nodeLens = _node nodeUuid
        outletLens = _seq outletUuid
        getter nw =
            view nodeLens nw
                >>= \(Node _ _ _ _ { outlets }) -> view outletLens outlets
        setter nw val =
            over nodeLens
                (map $ \(Node nuuid npath n process nstate@{ outlets }) ->
                    Node
                        nuuid
                        npath
                        n
                        process
                        nstate { outlets = set outletLens val outlets }
                ) nw

_nodeOutlets :: forall d c n. UUID.ToNode -> Getter' (Network d c n) (Maybe (Seq (Outlet d c)))
_nodeOutlets nodeUuid =
    to \nw ->
        view (_node nodeUuid) nw
            >>= \(Node _ _ _ _ { outlets }) ->
                pure $ viewOutlet nw <$> outlets # Util.seqCatMaybes
    where
        viewOutlet nw outletUuid = view (_outlet outletUuid) nw


_nodeOutletsByPath
    :: forall d c n
     . Path.ToNode
    -> Getter' (Network d c n) (Maybe (Seq (Outlet d c)))
_nodeOutletsByPath nodePath =
    to \nw ->
        view (_pathToId $ Path.lift nodePath) nw
            >>= UUID.toNode
            >>= \uuid -> view (_nodeOutlets uuid) nw


_inlet :: forall d c n. UUID.ToInlet -> Lens' (Network d c n) (Maybe (Inlet d c))
_inlet uuid = _uuidLens InletEntity extractInlet $ UUID.liftTagged uuid


_inletByPath :: forall d c n. Path.ToInlet -> Getter' (Network d c n) (Maybe (Inlet d c))
_inletByPath = Path.lift >>> _pathGetter extractInlet


_outlet :: forall d c n. UUID.ToOutlet -> Lens' (Network d c n) (Maybe (Outlet d c))
_outlet uuid = _uuidLens OutletEntity extractOutlet $ UUID.liftTagged uuid


_outletByPath :: forall d c n. Path.ToOutlet -> Getter' (Network d c n) (Maybe (Outlet d c))
_outletByPath = Path.lift >>> _pathGetter extractOutlet


_link :: forall d c n. UUID.ToLink -> Lens' (Network d c n) (Maybe Link)
_link uuid = _uuidLens LinkEntity extractLink $ UUID.liftTagged uuid


-- _linkByPath :: forall d. Path.ToLink -> Getter' (Network d) (Maybe Link)
-- _linkByPath = _pathGetter extractLink

-- TODO: only getter for Sequence?
_networkPatch :: forall d c n. UUID.ToPatch -> Lens' (Network d c n) (Maybe Unit)
_networkPatch uuid =
    lens getter setter
    where
        patchLens = _seq uuid
        getter (Network { patches }) =
            view patchLens patches
        setter (Network nwstate) val =
            Network
                nwstate
                    { patches = nwstate.patches # set patchLens val
                    }


_networkPatches :: forall d c n. Getter' (Network d c n) (List (Patch d c n))
_networkPatches =
    to \nw@(Network { patches }) ->
        (\uuid -> view (_patch uuid) nw)
            <$> List.fromFoldable patches
             #  List.catMaybes

_networkNodes :: forall d c n. Getter' (Network d c n) (List (Node d n))
_networkNodes =
    to \nw@(Network { registry }) ->
        List.mapMaybe extractNode $ Map.values registry


_networkInlets :: forall d c n. Getter' (Network d c n) (List (Inlet d c))
_networkInlets =
    to \nw@(Network { registry }) ->
        List.mapMaybe extractInlet $ Map.values registry


_networkOutlets :: forall d c n. Getter' (Network d c n) (List (Outlet d c))
_networkOutlets =
    to \nw@(Network { registry }) ->
        List.mapMaybe extractOutlet $ Map.values registry


_networkLinks :: forall d c n. Getter' (Network d c n) (List Link)
_networkLinks =
    to \nw@(Network { registry }) ->
        List.mapMaybe extractLink $ Map.values registry


_cancelers :: forall d c n. UUID -> Lens' (Network d c n) (Maybe (Array Canceler))
_cancelers uuid =
    lens getter setter
    where
        cancelersLens = at uuid
        getter (Network { cancelers }) =
            view cancelersLens cancelers
        setter (Network nwstate@{ cancelers }) val =
            Network
                nwstate
                    { cancelers = set cancelersLens val cancelers
                    }


_cancelersByPath :: forall d c n. Path -> Getter' (Network d c n) (Maybe (Array Canceler))
_cancelersByPath path =
    to \nw ->
        view (_pathToId path) nw
            >>= \uuid -> view (_cancelers $ UUID.uuid uuid) nw


_inletFlow :: forall d c n. UUID.ToInlet -> Getter' (Network d c n) (Maybe (InletFlow d))
_inletFlow inletId =
    to extractFlow
    where
        inletLens = _inlet inletId
        extractFlow nw = view inletLens nw >>=
            \(Inlet _ _ _ { flow }) -> pure flow


_outletFlow :: forall d c n. UUID.ToOutlet -> Getter' (Network d c n) (Maybe (OutletFlow d))
_outletFlow outletId =
    to extractFlow
    where
        inletLens = _outlet outletId
        extractFlow nw = view inletLens nw >>=
            \(Outlet _ _ _ { flow }) -> pure flow


_inletPush :: forall d c n. UUID.ToInlet -> Getter' (Network d c n) (Maybe (PushToInlet d))
_inletPush inletId =
    to extractPFlow
    where
        inletLens = _inlet inletId
        extractPFlow nw = view inletLens nw >>=
            \(Inlet _ _ _ { push }) -> pure push


_outletPush :: forall d c n. UUID.ToOutlet -> Getter' (Network d c n) (Maybe (PushToOutlet d))
_outletPush outletId =
    to extractPFlow
    where
        outletLens = _outlet outletId
        extractPFlow nw = view outletLens nw >>=
            \(Outlet _ _ _ { push }) -> pure push


-- FIXME: These below are Prisms: https://github.com/purescript-contrib/purescript-profunctor-lenses/blob/master/examples/src/PrismsForSumTypes.purs


extractPatch :: forall d c n. Entity d c n -> Maybe (Patch d c n)
extractPatch (PatchEntity pEntity) = Just pEntity
extractPatch _ = Nothing


extractNode :: forall d c n. Entity d c n -> Maybe (Node d n)
extractNode (NodeEntity nEntity) = Just nEntity
extractNode _ = Nothing


extractInlet :: forall d c n. Entity d c n -> Maybe (Inlet d c)
extractInlet (InletEntity iEntity) = Just iEntity
extractInlet _ = Nothing


extractOutlet :: forall d c n. Entity d c n -> Maybe (Outlet d c)
extractOutlet (OutletEntity oEntity) = Just oEntity
extractOutlet _ = Nothing


extractLink :: forall d c n. Entity d c n -> Maybe Link
extractLink (LinkEntity lEntity) = Just lEntity
extractLink _ = Nothing


-- allFromRegistry
--     :: forall d c n x
--      . (UUID.Tagged -> Boolean)
--     -> (Entity d c n -> Maybe x)
--     -> Network d c n
--     -> Seq x
-- allFromRegistry
--     checkUUID
--     extractEntity
--     (Network { registry })
--     =
--     Map.toUnfoldable registry
--         # Seq.filter (Tuple.fst >>> checkUUID)
--         # map Tuple.snd
--         # map extractEntity
--         # seqCatMaybes


-- same as `_networkNodes`
-- allNodes :: forall d c n. Network d c n -> Seq (Node d n)
-- allNodes = allFromRegistry UUID.isToNode extractNodeEntity


-- same as `_networkInlets`
-- allInlets :: forall d c n. Network d c n -> Seq (Inlet d c)
-- allInlets = allFromRegistry UUID.isToInlet extractInletEntity


-- same as `_networkOutlets`
-- allOutlets :: forall d c n. Network d c n -> Seq (Outlet d c)
-- allOutlets = allFromRegistry UUID.isToOutlet extractOutletEntity


-- same as `_networkLinks`
-- allLinks :: forall d c n. Network d c n -> Seq Link
-- allLinks = allFromRegistry UUID.isToLink extractLinkEntity
