module Rpd
    ( Rpd, RpdError, init
    , (</>), rpdBind, run, run', emptyNetwork
    --, RpdOp, RpdEffOp
    , Flow, flow
    , Network, Patch, Node, Inlet, Outlet, Link
    , PatchDef, NodeDef, InletDef, OutletDef, ProcessF(..)
    , Canceler, Subscriber, PushableFlow
    --, emptyNetwork
    --, network, patch, node, inlet, inlet', inletWithDefault, inletWithDefault', outlet, outlet'
    , connect, disconnectAll --, disconnectTop
    , addPatch, addPatch', addNode, addNode', addInlet, addInlet', addOutlet, addOutlet'
    , subscribeInlet, subscribeOutlet, subscribeAllInlets, subscribeAllOutlets
    , subscribeChannelsData, subscribeNode  -- subscribeAllData
    , sendToInlet, streamToInlet, sendToOutlet, streamToOutlet
    , PatchId(..), NodePath(..), InletPath(..), OutletPath(..), LinkId(..)
    , patchId, nodePath, inletPath, outletPath
    , isNodeInPatch, isInletInPatch, isOutletInPatch, isInletInNode, isOutletInNode
    , notInTheSameNode
    , getPatchOfNode, getPatchOfInlet, getPatchOfOutlet, getNodeOfInlet, getNodeOfOutlet
    --, findPatch, findNode, findOutlet, findInlet
    ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT, runExceptT, mapExceptT, except)
import Data.Array ((!!))
import Data.Array as Array
import Data.Bifunctor (lmap, bimap)
import Data.Bitraversable (bisequence)
import Data.Either (Either(..), either, note)
import Data.Foldable (fold, foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (Lens', Getter', lens, view, set, setJust, over, to)
import Data.Lens.At (at)
import Data.List (List, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (sequence, traverse, traverse_)
import Data.Tuple (curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect, foreachE)
import Effect.Class (liftEffect)
import FRP.Event (Event, filterMap)
import FRP.Event as E
import Rpd.Util (type (/->))
import Rpd.Util as RU
import Unsafe.Coerce (unsafeCoerce)


--import Rpd.Flow as Flow

-- data RunningNetwork d e = RpdEff e (Network d e)

data RpdError = RpdError String


type RpdOp a = Either RpdError a
--type RpdEffOp e a = RpdEff e (RpdOp e a)
type Rpd a = ExceptT RpdError Effect a
-- type Rpd d e = ContT (Either RpdError (Network d e)) (Eff (RpdEffE e)) (Network d e)
-- newtype ContT r m a = ContT ((a -> m r) -> m r)


infixl 1 rpdBind as </>
-- FIXME: can be replaced with proper instances?
-- other options: └, ~>, ...


run
    :: forall d
     . (RpdError -> Effect Unit)
    -> (Network d -> Effect Unit)
    -> Rpd (Network d)
    -> Effect Unit
run = run'


run'
    :: forall a r
     . (RpdError -> Effect r)
    -> (a -> Effect r)
    -> Rpd a
    -> Effect r
run' onError onSuccess rpd =
    runExceptT rpd >>= either onError onSuccess
    -- FIXME: we should also call all the cancelers left in the network, before "exiting"


rpdBind :: forall a b. Rpd a -> (a -> Rpd b) -> Rpd b
rpdBind = (>>=)


someApiFunc :: forall d. Rpd (Network d)
someApiFunc =
    init "t"
        </> addPatch "foo"
        </> addNode (PatchId 0) "test1"
        </> addNode (PatchId 0) "test2"


-- instance functorRpdOp :: Functor (RpdOp d) where
-- instance applyRpdOp :: Apply (RpdOp d) where
-- instance applicativeRpdOp :: Applicative (RpdOp d) where

-- instance functorRpdEffOp :: Functor (RpdEffOp d) where
-- instance applyRpdEffOp :: Apply (RpdEffOp d) where
-- instance applicativeRpdEffOp :: Applicative (RpdEffOp d) where


type Flow d = Event d
type PushF d = (d -> Effect Unit)
data PushableFlow d = PushableFlow (PushF d) (Flow d)


type Canceler =
    Effect Unit
type Subscriber =
    Effect Canceler


flow :: forall d. Event d -> Flow d
flow = identity


-- TODO: may be find better ways to process these things in future
--       I'd like to have something similar to JS-world
--       function (inlets) { return { 'c': inlets.a + inlets.b } }
-- variants:
--  `Record.set` / `Record.get` etc.
--  `Foreign.Object`` : https://github.com/purescript/purescript-foreign-object/blob/master/src/Foreign/Object.purs
--  `liftA2 (+) (m^.at a) (m^.at b)` -- Map -> Map

data ProcessF d
    = FlowThrough
    | IndexBased (Array d -> Array d)
    | LabelBased ((String /-> d) -> (String /-> d))


data PatchId = PatchId Int
data NodePath = NodePath PatchId Int
data InletPath = InletPath NodePath Int
data OutletPath = OutletPath NodePath Int
data LinkId = LinkId Int


-- data DataSource d
--     = UserSource (Flow d)
--     | OutletSource OutletPath (Flow d)

type PatchDef d =
    { name :: String
    , nodeDefs :: List (NodeDef d)
    }
type NodeDef d =
    { name :: String
    , inletDefs :: List (InletDef d)
    , outletDefs :: List (OutletDef d)
    , process :: ProcessF d
    }
type InletDef d =
    { label :: String
    , default :: Maybe d
    , accept :: Maybe (d -> Boolean)
    --, adapt ::
    }
type OutletDef d =
    { label :: String
    }


data Network d =
    Network
        { name :: String
        , patchDefs :: List (PatchDef d)
        }
        { patches :: PatchId /-> Patch d
        , nodes :: NodePath /-> Node d
        , inlets :: InletPath /-> Inlet d
        , outlets :: OutletPath /-> Outlet d
        , links :: LinkId /-> Link
        , cancelers ::
            { links :: LinkId /-> Canceler
            , nodes :: NodePath /-> Canceler
            , inlets :: InletPath /-> Canceler
            }
        }
data Patch d =
    Patch
        PatchId
        (PatchDef d)
        { nodes :: Set NodePath
        }
data Node d =
    Node
        NodePath -- (NodeDef d)
        (NodeDef d)
        { inlets :: Set InletPath
        , outlets :: Set OutletPath
        , flow :: PushableFlow (InletPath /\ d)
        , processFlow :: Flow (InletPath /-> d)
        }
data Inlet d =
    Inlet
        InletPath
        (InletDef d)
        { flow :: PushableFlow d
        -- sources :: Set (DataSource d)
        }
data Outlet d =
    Outlet
        OutletPath
        (OutletDef d)
        { flow :: PushableFlow d
        }
data Link = Link OutletPath InletPath


init :: forall d. String -> Rpd (Network d)
init = pure <<< emptyNetwork


emptyNetwork :: forall d. String -> Network d
emptyNetwork name =
    Network
        { name
        , patchDefs : List.Nil
        }
        { patches : Map.empty
        , nodes : Map.empty
        , inlets : Map.empty
        , outlets : Map.empty
        , links : Map.empty
        , cancelers :
            { links : Map.empty
            , inlets : Map.empty
            , nodes : Map.empty
            }
        }


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


_nodeFlow :: forall d. NodePath -> Getter' (Network d) (Maybe (Flow (InletPath /\ d)))
_nodeFlow nodePath =
    to extractFlow
    where
        pFlowLens = _nodePFlow nodePath
        extractFlow nw = view pFlowLens nw >>=
            \(PushableFlow _ flow) -> pure flow


_nodePFlow :: forall d. NodePath -> Getter' (Network d) (Maybe (PushableFlow (InletPath /\ d)))
_nodePFlow nodePath =
    to extractPFlow
    where
        nodeLens = _node nodePath
        extractPFlow nw = view nodeLens nw >>=
            \(Node _ _ { flow }) -> pure flow



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


_inletPFlow :: forall d. InletPath -> Getter' (Network d) (Maybe (PushableFlow d))
_inletPFlow inletPath =
    to extractPFlow
    where
        inletLens = _inlet inletPath
        extractPFlow nw = view inletLens nw >>=
            \(Inlet _ _ { flow }) -> pure flow


_inletFlow :: forall d. InletPath -> Getter' (Network d) (Maybe (Flow d))
_inletFlow inletPath =
    to extractFlow
    where
        pFlowLens = _inletPFlow inletPath
        extractFlow nw = view pFlowLens nw >>=
            \(PushableFlow _ flow) -> pure flow


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
        extractPFlow nw = view outletLens nw >>=
            \(Outlet _ _ { flow }) -> pure flow


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


makePushableFlow :: forall d. Effect (PushableFlow d)
makePushableFlow = do
    { push, event } <- E.create
    pure $ PushableFlow push event


nextPatchId :: forall d. Network d -> PatchId
nextPatchId (Network _ { patches }) =
    PatchId (Map.size patches)


nextNodePath :: forall d. PatchId -> Network d -> Either RpdError NodePath
nextNodePath patchId (Network _ { patches }) = do
    (Patch _ _ { nodes }) <- Map.lookup patchId patches
                                # note (RpdError "")
    pure $ NodePath patchId $ Set.size nodes


nextInletPath :: forall d. NodePath -> Network d -> Either RpdError InletPath
nextInletPath nodePath (Network _ { nodes }) = do
    (Node _ _ { inlets }) <- Map.lookup nodePath nodes
                                # note (RpdError "")
    pure $ InletPath nodePath $ Set.size inlets


nextOutletPath :: forall d. NodePath -> Network d -> Either RpdError OutletPath
nextOutletPath nodePath (Network _ { nodes }) = do
    (Node _ _ { outlets }) <- Map.lookup nodePath nodes
                                # note (RpdError "")
    pure $ OutletPath nodePath $ Set.size outlets


nextLinkId :: forall d. Network d -> LinkId
nextLinkId (Network _ { links }) =
    LinkId (Map.size links)



exceptMaybe :: forall a. RpdError -> Maybe a -> ExceptT RpdError Effect a
exceptMaybe err maybe =
    except (maybe # note err)


addPatch :: forall d. String -> Network d -> Rpd (Network d)
addPatch name =
    addPatch'
        { name
        , nodeDefs : List.Nil
        }


addPatch'
    :: forall d
     . PatchDef d
    -> Network d
    -> Rpd (Network d)
addPatch' patchDef nw =
    pure $ setJust (_patch patchId) newPatch nw
    where
        patchId = nextPatchId nw
        newPatch =
            Patch
                patchId
                patchDef
                { nodes : Set.empty
                }

-- TODO: removePatch
    -- TODO: cancel all the cancelers related to the patch

addNode
    :: forall d
     . PatchId
    -> String
    -> Network d
    -> Rpd (Network d)
addNode patchId name =
    addNode'
        patchId
        { name
        , inletDefs : List.Nil
        , outletDefs : List.Nil
        , process : FlowThrough
        }


addNode'
    :: forall d
     . PatchId
    -> NodeDef d
    -> Network d
    -> Rpd (Network d)
addNode' patchId def nw = do
    nodePath <- except $ nextNodePath patchId nw
    dataPFlow@(PushableFlow _ dataFlow) <- liftEffect makePushableFlow
    let
        processFlow = makeProcessFlow dataFlow
        newNode =
            Node
                nodePath
                def
                { inlets : Set.empty
                , outlets : Set.empty
                , flow : dataPFlow
                , processFlow : processFlow -- must be an event doing nothing at first
                }
    nw
         #  setJust (_node nodePath) newNode
         #  setJust (_patchNode patchId nodePath) unit
         #  addInlets nodePath def.inletDefs
        </> addOutlets nodePath def.outletDefs
        </> updateNodeProcessFlow nodePath


updateNodeProcessFlow
    :: forall d
     . NodePath
    -> Network d
    -> Rpd (Network d)
updateNodeProcessFlow nodePath nw = do
    -- cancel the previous subscription if it exists
    _ <- liftEffect $ fromMaybe (pure unit) $ view (_nodeCanceler nodePath) nw
    (Node _ nodeDef { processFlow, inlets, outlets }) <-
        except $ view (_node nodePath) nw # note (RpdError "")
    if (isProcessNeeded nodeDef.process
        || Set.isEmpty inlets
        || Set.isEmpty outlets) then pure nw else do
        (PushableFlow _ dataFlow) <- except $ view (_nodePFlow nodePath) nw # note (RpdError "")
        let
            (processHandler :: (InletPath /-> d) -> Effect Unit) =
                nw # findFittingProcessHandler nodeDef.process outlets
        canceler :: Canceler
            <- liftEffect $ E.subscribe processFlow processHandler
        pure $ nw # setJust (_nodeCanceler nodePath) canceler
    where
        isProcessNeeded FlowThrough = false
        isProcessNeeded _ = true


findFittingProcessHandler
    :: forall d
     . ProcessF d
    -> Set OutletPath
    -> Network d
    -> (InletPath /-> d)
    -> Effect Unit
findFittingProcessHandler (LabelBased processF) outlets nw =
    let
        (outletLabelToFlow :: String /-> PushableFlow d) =
            outlets
                # (Set.toUnfoldable :: forall a. Set a -> Array a)
                # map (\outletPath ->
                    view (_outlet outletPath) nw)
                # filterMap (\maybeOutlet ->
                    maybeOutlet >>= \(Outlet _ { label } { flow }) ->
                        pure (label /\ flow))
                # Map.fromFoldable
        ph = nw # makeProcessHandler processF outletLabelToFlow
        pathToLabel inletPath = -- FIXME
            fromMaybe "<?>" $ view (_inletLabel inletPath) nw
    in
        ph <<< RU.convertKeysInMap pathToLabel
findFittingProcessHandler (IndexBased processF) outlets nw =
    let
        (outletFlows :: Array (PushableFlow d)) =
            outlets
                # (Set.toUnfoldable :: forall a. Set a -> Array a)
                # map (\outletPath -> view (_outletPFlow outletPath) nw)
                # filterMap identity
        ph = nw # makeProcessHandler' processF outletFlows
    in
        ph <<< List.toUnfoldable <<< Map.values
findFittingProcessHandler FlowThrough outlets nw =
    unsafeCoerce



-- makeProcessHandler
--     :: forall d
--      . (String /-> d -> String /-> d)
--     -> (String /-> PushableFlow d)
--     -> Network d
--     -> (String /-> d)
--     -> Effect Unit
-- makeProcessHandler processF outletFlows nw inletVals = do

-- makeProcessHandler'
--     :: forall d
--      . (Array d -> Array d)
--     -> Array (PushableFlow d)
--     -> Network d
--     -> Array d
--     -> Effect Unit
-- makeProcessHandler processF outletFlows nw inletVals = do


-- TODO: removeNode
    -- execute process canceler
    -- execute node' inlets cancelers
    -- execute cancelers for the links going into and out of the node


addInlet
    :: forall d
     . NodePath
    -> String
    -> Network d
    -> Rpd (Network d)
addInlet nodePath label =
    addInlet'
        nodePath
        { label
        , default : Nothing
        , accept : Nothing
        }

addInlet'
    :: forall d
     . NodePath
    -> InletDef d
    -> Network d
    -> Rpd (Network d)
addInlet' nodePath def nw = do
    inletPath <- except $ nextInletPath nodePath nw
    -- TODO: when there's already some inlet exists with the same path,
    -- cancel its subscription before
    pushableFlow@(PushableFlow pushData dataFlow) <- liftEffect makePushableFlow
    (Node _ _ { flow }) :: Node d <- view (_node nodePath) nw # exceptMaybe (RpdError "")
    let (PushableFlow pushNodeData _ ) = flow
    let
        newInlet =
            Inlet
                inletPath
                def
                { flow : pushableFlow
                }
    canceler :: Canceler <-
        liftEffect $
            E.subscribe dataFlow (\d -> pushNodeData (inletPath /\ d))
    nw # setJust (_inlet inletPath) newInlet
       # setJust (_nodeInlet nodePath inletPath) unit
       # setJust (_inletCanceler inletPath) canceler
       # updateNodeProcessFlow nodePath


addInlets :: forall d. NodePath -> List (InletDef d) -> Network d -> Rpd (Network d)
addInlets nodePath inletDefs nw =
    -- FIXME: may appear not very optimal, since every `addInlet'`
    --        call looks for the node again and again
    foldr foldingF (pure nw) inletDefs
    where
        foldingF inletDef rpd =
            rpd </> addInlet' nodePath inletDef


-- TODO: removeInlet
    -- TODO: execute the corresponding process canceler
    -- TODO: cancel all the links going into this inlet
    -- TODO: updateNodeProcessFlow


addOutlet
    :: forall d
     . NodePath
    -> String
    -> Network d
    -> Rpd (Network d)
addOutlet nodePath label =
    addOutlet'
        nodePath
        { label
        }


addOutlet'
    :: forall d
     . NodePath
    -> OutletDef d
    -> Network d
    -> Rpd (Network d)
addOutlet' nodePath def nw = do
    outletPath <- except $ nextOutletPath nodePath nw
    pushableFlow <- liftEffect makePushableFlow
    let
        newOutlet =
            Outlet
                outletPath
                def
                { flow : pushableFlow
                }
    nw # setJust (_outlet outletPath) newOutlet
       # setJust (_nodeOutlet nodePath outletPath) unit
       # updateNodeProcessFlow nodePath


addOutlets :: forall d. NodePath -> List (OutletDef d) -> Network d -> Rpd (Network d)
addOutlets nodePath outletDefs nw =
    -- FIXME: may appear not very optimal, since every `addOutlet'`
    --        call looks for the node again and again
    foldr foldingF (pure nw) outletDefs
    where
        foldingF outletDef rpd =
            rpd </> addOutlet' nodePath outletDef


-- TODO: removeOutlet
    -- TODO: cancel all the links going from this outlet
    -- TODO: updateNodeProcessFlow


sendToInlet
    :: forall d
     . InletPath
    -> d
    -> Network d
    -> Rpd (Network d)
sendToInlet inletPath d nw = do
    (PushableFlow push _) <-
        view (_inletPFlow inletPath) nw # exceptMaybe (RpdError "")
    _ <- liftEffect $ push d
    pure nw


streamToInlet
    :: forall d
     . InletPath
    -> Flow d
    -> Network d
    -> Rpd Canceler
streamToInlet inletPath flow nw = do
    (PushableFlow push _) <-
        view (_inletPFlow inletPath) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <-
        liftEffect $ E.subscribe flow push
    pure canceler


sendToOutlet
    :: forall d
     . OutletPath
    -> d
    -> Network d
    -> Rpd (Network d)
sendToOutlet outletPath d nw = do
    (PushableFlow push _) <-
        view (_outletPFlow outletPath) nw # exceptMaybe (RpdError "")
    _ <- liftEffect $ push d
    pure nw


streamToOutlet
    :: forall d
     . OutletPath
    -> Flow d
    -> Network d
    -> Rpd Canceler
streamToOutlet outletPath flow nw = do
    (PushableFlow push _) <-
        view (_outletPFlow outletPath) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <-
        liftEffect $ E.subscribe flow push
    pure canceler


subscribeInlet
    :: forall d
     . InletPath
    -> (d -> Effect Unit)
    -> Network d
    -> Rpd Canceler
subscribeInlet inletPath handler nw = do
    flow :: Flow d <-
        view (_inletFlow inletPath) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <- liftEffect $ E.subscribe flow handler
    pure canceler


subscribeOutlet
    :: forall d
     . OutletPath
    -> (d -> Effect Unit)
    -> Network d
    -> Rpd Canceler
subscribeOutlet outletPath handler nw = do
    flow :: Flow d <-
        view (_outletFlow outletPath) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <-
        liftEffect $ E.subscribe flow handler
    pure canceler


subscribeAllInlets
    :: forall d
     . (InletPath -> d -> Effect Unit)
    -> Network d
    -> Effect (InletPath /-> Canceler)
subscribeAllInlets handler (Network _ { inlets }) =
    traverse sub inlets
    where
        sub :: Inlet d -> Subscriber
        sub (Inlet inletPath _ { flow }) =
            case flow of
                PushableFlow _ fl -> E.subscribe fl $ handler inletPath


subscribeAllOutlets
    :: forall d
     . (OutletPath -> d -> Effect Unit)
    -> Network d
    -> Effect (OutletPath /-> Canceler)
subscribeAllOutlets handler (Network _ { outlets }) =
    traverse sub outlets
    where
        sub :: Outlet d -> Subscriber
        sub (Outlet outletPath _ { flow }) =
            case flow of
                PushableFlow _ fl -> E.subscribe fl $ handler outletPath


subscribeChannelsData
    :: forall d
     . (OutletPath -> d -> Effect Unit)
    -> (InletPath -> d -> Effect Unit)
    -> Network d
    -> Effect ((OutletPath /-> Canceler) /\ (InletPath /-> Canceler))
subscribeChannelsData oHandler iHandler nw =
    bisequence $ subscribeAllOutlets oHandler nw /\ subscribeAllInlets iHandler nw


subscribeNode
    :: forall d
     . NodePath
    -> (InletPath /\ d -> Effect Unit)
    -> Network d
    -> Rpd Canceler
subscribeNode nodePath handler nw = do
    flow :: Flow (InletPath /\ d) <-
        view (_nodeFlow nodePath) nw
            # exceptMaybe (RpdError "")
    canceler :: Canceler <-
        liftEffect $ E.subscribe flow handler
    pure canceler


connect
    :: forall d
     . OutletPath
    -> InletPath
    -> Network d
    -> Rpd (Network d)
-- FIXME: rewrite for the case of different patches
connect outletPath inletPath
    nw@(Network nwdef nwstate) = do
    let
        linkId = nextLinkId nw
        newLink = Link outletPath inletPath
        iNodePath = getNodeOfInlet inletPath
        oPatchId = getPatchOfOutlet outletPath
        iPatchId = getPatchOfInlet inletPath

    outletPFlow <- view (_outletPFlow outletPath) nw # exceptMaybe (RpdError "")
    inletPFlow <- view (_inletPFlow inletPath) nw # exceptMaybe (RpdError "")

    let
        (PushableFlow _ outletFlow) = outletPFlow
        (PushableFlow pushToInlet inletFlow) = inletPFlow

    linkCanceler :: Canceler <-
            liftEffect $
                E.subscribe outletFlow pushToInlet

    pure $ nw
            # setJust (_link linkId) newLink
            # setJust (_linkCanceler linkId) linkCanceler


disconnectAll
    :: forall d
     . OutletPath
    -> InletPath
    -> Network d
    -> Rpd (Network d)
disconnectAll outletPath inletPath
    nw@(Network nwdef nwstate@{ links }) = do
    let
        linkForDeletion (Link outletPath' inletPath') =
            (outletPath' == outletPath) && (inletPath' == inletPath)
        linksForDeletion = Map.keys $ links # Map.filter linkForDeletion

        oPatchId = getPatchOfOutlet outletPath
        iPatchId = getPatchOfInlet inletPath

    _ <- liftEffect $ traverse_
            (\linkId -> fromMaybe (pure unit) $ view (_linkCanceler linkId) nw)
            linksForDeletion

    pure $ (
        foldr (\linkId nw ->
            nw # set (_link linkId) Nothing
               # set (_linkCanceler linkId) Nothing
        ) nw linksForDeletion
        -- # setJust (_inletConnections inletPath) newInletConnections
        -- # setJust (_outletConnections outletPath) newOutletConnections
    )

    -- TODO: un-subscribe `process`` function of the target node to update values including this connection

-- TODO: disconnectTop

-- TODO: disconnectTopOf (OutletPath /\ InletPath)


-- TODO: subscribeAllNodes


-- TODO: subscribeAllData


makeProcessFlow :: forall d. Flow (InletPath /\ d) -> Flow (InletPath /-> d)
makeProcessFlow dataFlow =
    E.fold (uncurry Map.insert) dataFlow Map.empty


makeProcessHandler
    :: forall d
     . (String /-> d -> String /-> d)
    -> (String /-> PushableFlow d)
    -> Network d
    -> (String /-> d)
    -> Effect Unit
makeProcessHandler processF outletFlows nw inletVals = do
    let (outletVals :: String /-> d) = processF inletVals
    foreachE (Set.toUnfoldable $ Map.keys outletVals) (pushToOutlet outletVals)
    pure unit
    where
        getInletId (InletPath _ inletId) = inletId
        pushToOutlet outletVals outletLabel =
            case view (at outletLabel) outletFlows of
                Just (PushableFlow push _) -> do
                    maybe
                        (pure unit)
                        (\d -> do
                            _ <- push d
                            pure unit
                        ) $ view (at outletLabel) outletVals
                Nothing -> do
                    pure unit


makeProcessHandler'
    :: forall d
     . (Array d -> Array d)
    -> Array (PushableFlow d)
    -> Network d
    -> Array d
    -> Effect Unit
makeProcessHandler' processF outletFlows nw inletVals = do
    let (outletVals :: Array d) = processF inletVals
    _ <- sequence $ Array.mapWithIndex pushToOutlet outletVals
    pure unit
    where
        pushToOutlet outletIndex d =
            case outletFlows !! outletIndex of
                Just (PushableFlow push _) -> do
                    _ <- push d
                    pure unit
                Nothing -> do
                    pure unit



isNodeInPatch :: NodePath -> PatchId -> Boolean
isNodeInPatch (NodePath patchId' _) patchId =
    patchId == patchId'


isInletInPatch :: InletPath -> PatchId -> Boolean
isInletInPatch (InletPath nodePath _) patchId =
    isNodeInPatch nodePath patchId


isOutletInPatch :: OutletPath -> PatchId -> Boolean
isOutletInPatch (OutletPath nodePath _) patchId =
    isNodeInPatch nodePath patchId


isInletInNode :: InletPath -> NodePath -> Boolean
isInletInNode (InletPath nodePath' _) nodePath =
    nodePath == nodePath'


isOutletInNode :: OutletPath -> NodePath -> Boolean
isOutletInNode (OutletPath nodePath' _) nodePath =
    nodePath == nodePath'


notInTheSameNode :: InletPath -> OutletPath -> Boolean
notInTheSameNode (InletPath iNodePath _) (OutletPath oNodePath _) =
    iNodePath /= oNodePath


patchId :: Int -> PatchId
patchId = PatchId


nodePath :: Int -> Int -> NodePath
nodePath pId nId = NodePath (PatchId pId) nId


inletPath :: Int -> Int -> Int -> InletPath
inletPath pId nId iId = InletPath (NodePath (PatchId pId) nId) iId


outletPath :: Int -> Int -> Int -> OutletPath
outletPath pId nId iId = OutletPath (NodePath (PatchId pId) nId) iId


-- FIXME: below are Lenses/Prisms

getPatchOfNode :: NodePath -> PatchId
getPatchOfNode (NodePath pId _) = pId


getPatchOfInlet :: InletPath -> PatchId
getPatchOfInlet inlet = getPatchOfNode $ getNodeOfInlet inlet


getPatchOfOutlet :: OutletPath -> PatchId
getPatchOfOutlet outlet = getPatchOfNode $ getNodeOfOutlet outlet


getNodeOfInlet :: InletPath -> NodePath
getNodeOfInlet  (InletPath nPath _) = nPath


getNodeOfOutlet :: OutletPath -> NodePath
getNodeOfOutlet  (OutletPath nPath _) = nPath


unpackNodePath :: NodePath -> Array Int
unpackNodePath (NodePath (PatchId patchId) id) = [ patchId, id ]

unpackInletPath :: InletPath -> Array Int
unpackInletPath (InletPath nodePath id) = unpackNodePath nodePath <> [ id ]

unpackOutletPath :: OutletPath -> Array Int
unpackOutletPath (OutletPath nodePath id) = unpackNodePath nodePath <> [ id ]



instance showRpdError :: Show RpdError where
    show (RpdError text) = "Error: " <> text


instance showPatchId :: Show PatchId where
    show (PatchId id) = "P" <> show id

instance showNodePath :: Show NodePath where
    show (NodePath patchId id) = show patchId <> "/N" <> show id

instance showInletPath :: Show InletPath where
    show (InletPath nodePath id) = show nodePath <> "/I" <> show id

instance showOutletPath :: Show OutletPath where
    show (OutletPath nodePath id) = show nodePath <> "/O" <> show id

instance showLinkId :: Show LinkId where
    show (LinkId id) = "L" <> show id

instance showLink :: Show Link where
    show (Link outletPath inletPath) = "Link " <> show outletPath <> " -> " <> show inletPath


instance eqPatchId :: Eq PatchId where
    eq (PatchId a) (PatchId b) = a == b

instance eqNodePath :: Eq NodePath where
    eq (NodePath pa a) (NodePath pb b) = (pa == pb) && (a == b)

instance eqInletPath :: Eq InletPath where
    eq (InletPath na a) (InletPath nb b) = (na == nb) && (a == b)

instance eqOutletPath :: Eq OutletPath where
    eq (OutletPath na a) (OutletPath nb b) = (na == nb) && (a == b)

instance eqLinkId :: Eq LinkId where
    eq (LinkId a) (LinkId b) = a == b

-- instance eqDataSource :: Eq (DataSource d) where
--     eq (OutletSource oa a) (OutletSource ob b) = oa == ob
--     eq _ _ = false


instance ordPatchId :: Ord PatchId where
    compare (PatchId a) (PatchId b) = compare a b

instance ordNodePath :: Ord NodePath where
    compare nodePath1 nodePath2 =
        compare (unpackNodePath nodePath1)  (unpackNodePath nodePath2)

instance ordInletPath :: Ord InletPath where
    compare inletPath1 inletPath2 =
        compare (unpackInletPath inletPath1)  (unpackInletPath inletPath2)

instance ordOutletPath :: Ord OutletPath where
    compare outletPath1 outletPath2 =
        compare (unpackOutletPath outletPath1) (unpackOutletPath outletPath2)

instance ordLinkId :: Ord LinkId where
    compare (LinkId a) (LinkId b) =
        compare a b

-- instance ordDataSource :: Ord (DataSource d) where
--     compare (OutletSource oa a) (OutletSource ob b) = compare oa ob
--     compare _ _ = LT


-- TODO: create HasId / HasPath typeclass
instance eqPatch :: Eq (Patch d) where
    eq (Patch idA _ _) (Patch idB _ _) = (idA == idB)

instance eqNode :: Eq (Node d) where
    eq (Node pathA _ _) (Node pathB _ _) = (pathA == pathB)

instance eqInlet :: Eq (Inlet d) where
    eq (Inlet pathA _ _) (Inlet pathB _ _) = (pathA == pathB)

instance eqOutlet :: Eq (Outlet d) where
    eq (Outlet pathA _ _) (Outlet pathB _ _) = (pathA == pathB)

instance eqLink :: Eq Link where
    eq (Link outletA inletA) (Link outletB inletB) = (outletA == outletB) && (inletA == inletB)
