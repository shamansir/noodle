module Web.Components.PatchArea where

import Prelude

import Debug as Debug

import Type.Proxy (Proxy(..))

import Effect.Class (class MonadEffect)

import Control.Monad.State (get, put, modify, modify_) as State
import Control.Monad.Extra (whenJust)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (Map)
import Data.Map (empty, lookup, insert, size, fromFoldable, toUnfoldable, values) as Map
import Data.Map.Extra (update') as MapX
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (sortWith, find) as Array
import Data.Set (Set)
import Data.Set (empty, insert, member, fromFoldable) as Set
import Data.Int (toNumber) as Int
import Data.Bifunctor (lmap)
import Data.Foldable (foldl, foldr)
import Data.Newtype (unwrap) as NT
import Data.Text.Format (Tag) as T

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Properties.Extra (Position(..), position, position_) as HHP
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.Color as HC
import Halogen.Svg.Attributes.Color.Extra as HCColorX
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.Extra as HSX

import Web.UIEvent.MouseEvent (clientX, clientY) as Mouse
import Web.UIEvent.WheelEvent (deltaX, deltaY) as Wheel
import DOM.HTML.Indexed.InputType (InputType(..)) as I
import DOM.HTML.Indexed.StepValue (StepValue(..)) as I

import Noodle.Id (NodeR, InletR, OutletR, LinkR, FamilyR) as Id
import Noodle.Toolkit (class MarkToolkit, class HasChRepr)
import Noodle.Fn.Signature (class PossiblyToSignature)
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Link (id, connector) as RawLink
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (NodeChanges, id, shape, sendIn) as RawNode
import Noodle.Raw.Fn.Shape as RawShape
import Noodle.Repr.ChRepr (class WriteChannelRepr)
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.ValueInChannel (toFallback) as ViC
import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette
import Noodle.Ui.Tagging.At (ChannelLabel, StatusLine) as At
import Noodle.Ui.Tagging.At (class At) as T

import Web.Bounds (Bounds)
import Web.Bounds (getPosition, getSize) as Bounds
import Web.Layer (TargetLayer(..))
import Web.Components.NodeBox as NodeBox
import Web.Components.Link as LinkCmp
import Web.Components.ValueEditor as ValueEditor
import Web.Class.WebRenderer (class WebLocator, ConstantShift, class WebEditor, spawnWebEditor)
import Web.Class.WebRenderer (firstLocation, locateNext) as Web


newtype NodeZIndex = ZIndex Int
derive newtype instance Eq NodeZIndex
derive newtype instance Ord NodeZIndex
instance Bounded NodeZIndex where
    top = ZIndex 1000
    bottom = ZIndex 0


type Locator = ConstantShift -- TODO: move to some root App config?


type Slots sr cr =
    ( nodeBox :: H.Slot (NodeBox.Query sr cr) (NodeBox.Output cr) Id.NodeR
    , link :: forall q. H.Slot q LinkCmp.Output Id.LinkR
    , valueEditor :: H.Slot ValueEditor.Query (ValueEditor.Output cr) ValueEditor.EditorId
    )


_nodeBox = Proxy :: _ "nodeBox"
_link = Proxy :: _ "link"
_valueEditor = Proxy :: _ "valueEditor"


defaultPosition = { left : 0.0, top : 0.0 }


type LinkStart =
    { fromNode :: Id.NodeR
    , fromOutlet :: Id.OutletR
    }


type LinkEnd =
    { toNode :: Id.NodeR
    , toInlet :: Id.InletR
    }


data LockingTask
    = NoLock
    | DraggingNode Id.NodeR
    | Connecting LinkStart { x :: Number, y :: Number }


type NodesBounds = Map Id.NodeR (Bounds /\ NodeZIndex)


type State loc ps sr cr m =
    { offset :: { left :: Number, top :: Number }
    , size :: { width :: Number, height :: Number }
    , zoom :: Number
    , bgOpacity :: Number
    {- OUT -} , lastLocation :: loc
    , nodes :: Array (Raw.Node sr cr m) -- TODO: store nodes in a Map? do we need order except ZIndex?
    , nodesBounds :: NodesBounds
    , links :: Array Raw.Link
    {- OUT -} , lockOn :: LockingTask
    {- OUT -} , focusedNodes :: Set Id.NodeR
    , mbState :: Maybe ps
    , mbCurrentEditor :: Maybe (Id.NodeR /\ ValueEditor.Def cr)
        -- FIXME: since there could be only one value editor in the `App`, we have it `AppScreeen` state,
        --        but here we have access to all the nodes and rendering happens inside `PatchArea`,
        --        so we store it twice for now, solve it later somehow
    }


type Input ps sr cr m =
    { offset :: { left :: Number, top :: Number }
    , size :: { width :: Number, height :: Number }
    , zoom :: Number
    , bgOpacity :: Number
    , nodes :: Array (Raw.Node sr cr m)
    , nodesBounds :: NodesBounds
    , links :: Array Raw.Link
    , mbState :: Maybe ps
    , mbCurrentEditor :: Maybe (Id.NodeR /\ ValueEditor.Def cr)
    }


data Action ps sr cr m
    = Initialize
    | Receive (Input ps sr cr m)
    | PassUpdate Id.NodeR (RawNode.NodeChanges sr cr)
    | PatchAreaMouseMove { x :: Number, y :: Number }
    | WheelChange { dx :: Number, dy :: Number }
    | PatchAreaClick
    | FromNodeBox Id.NodeR (NodeBox.Output cr)
    | FromLink Id.LinkR LinkCmp.Output
    | FromValueEditor Id.NodeR Id.InletR (ValueEditor.Output cr)


data Output loc cr
    = Connect (LinkStart /\ LinkEnd)
    | Disconnect Id.LinkR
    | RemoveNode Id.NodeR
    | UpdateStatusBar T.Tag
    | ClearStatusBar
    | TryZoom Number
    | RequestValueEditor Id.NodeR (ValueEditor.Def cr)
    | CloseValueEditor
    -- | LocationUpdate loc
    | NewNodeBounds Id.NodeR loc Bounds -- TODO move to `AppScreen`
    | MoveNode Id.NodeR { left :: Number, top :: Number }
    -- | FocusUpdate (Set Id.NodeR)
    | LockUpdate LockingTask


data Query sr cr m a
    = ApplyNewNode (Raw.Node sr cr m) a
    | ApplyUpdate Id.NodeR (RawNode.NodeChanges sr cr) a
    | CancelConnecting a
    | ValueEditorClosedByUser a


component
    :: forall tk loc ps sr cr m
     . MonadEffect m
    => WebLocator loc
    => MarkToolkit tk
    => HasFallback cr
    => HasChRepr tk cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => T.At At.StatusLine cr
    => T.At At.ChannelLabel cr
    => WebEditor tk cr m
    => Proxy tk
    -> Proxy loc
    -> TargetLayer
    -> H.Component (Query sr cr m) (Input ps sr cr m) (Output loc cr) m
component ptk ploc trg =
    H.mkComponent
        { initialState : initialState ploc
        , render : render trg ptk
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            , receive = Just <<< Receive
            , handleQuery = handleQuery
            }
        }


initialState :: forall loc ps sr cr m. WebLocator loc => Proxy loc -> Input ps sr cr m -> State loc ps sr cr m
initialState _ { mbState, offset, size, zoom, bgOpacity, nodes, nodesBounds, links, mbCurrentEditor } =
    { lastLocation : Web.firstLocation
    , mbState
    , offset
    , size
    , zoom
    , bgOpacity
    , nodes
    , links
    , nodesBounds
    , lockOn : NoLock
    , focusedNodes : Set.empty
    , mbCurrentEditor
    }


render
    :: forall loc tk ps sr cr m
     . MonadEffect m
    => MarkToolkit tk
    => HasChRepr tk cr
    => HasFallback cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => T.At At.StatusLine cr
    => T.At At.ChannelLabel cr
    => WebEditor tk cr m
    => TargetLayer
    -> Proxy tk
    -> State loc ps sr cr m
    -> H.ComponentHTML (Action ps sr cr m) (Slots sr cr) m
render SVG ptk state =
    HS.g
        []
        [ HS.rect
            [ HSA.width state.size.width, HSA.height state.size.height
            , HSA.fill $ Just backgroundColor -- FIXME: `bgOpacity` for PatchArea & AppScreen multiples
            , HE.onClick $ const PatchAreaClick
            , HE.onMouseMove \mevt -> PatchAreaMouseMove
                { x : ((Int.toNumber $ Mouse.clientX mevt) - state.offset.left) / state.zoom
                , y : ((Int.toNumber $ Mouse.clientY mevt) - state.offset.top)  / state.zoom
                }
            , HE.onWheel \wevt -> WheelChange
                { dx : Wheel.deltaX wevt
                , dy : Wheel.deltaY wevt
                }
            ]
        , HS.g
            ( if state.zoom /= 1.0
                then [ HSA.transform [ HSA.Scale state.zoom state.zoom ] ]
                else [ ]
            )
            [ HS.g
                [ HE.onClick $ const PatchAreaClick
                ]
                $ nodeBoxesSlots
            , HS.g
                [  ]
                $ linksSlots
            , case state.lockOn of
                Connecting { fromNode, fromOutlet } mousePos ->
                    notYetConnectedLink
                        { from : outletPos $ fromNode /\ fromOutlet
                        , to : mousePos
                        }
                _ -> HSX.none
            ]
        ]
    where
        backgroundColor = fromMaybe (P.hColorOf Palette.black) $ HCColorX.setAlpha state.bgOpacity $ P.hColorOf Palette.black
        notYetConnectedLink = LinkCmp.linkShapeNotYetConnected
        nodesWithCells = _makeNodesWithCells state
        nodesToCellsMap = _makeNodesToCellsMap state
        nodeBoxesSlots = (nodesWithCells # Array.sortWith _.zIndex <#> nodeBoxSlot)
        linksSlots = state.links <#> linkSlot
        inletPos = _inletPosition nodesToCellsMap
        outletPos = _outletPosition nodesToCellsMap
        nodeBoxSlot { rawNode, position, inFocus, size } =
            let
                nodeR = RawNode.id rawNode
            in HH.slot _nodeBox nodeR (NodeBox.component ptk)
                { node : rawNode
                , position
                , size
                , inFocus
                }
                $ FromNodeBox nodeR
        handleLinkEvents = case state.lockOn of
            NoLock -> true
            DraggingNode _ -> false
            Connecting _ _ -> false
        linkSlot rawLink =
            let
                linkR = RawLink.id rawLink
                connector = RawLink.connector rawLink
            in HH.slot _link linkR LinkCmp.component
                { connector
                , id : linkR
                , position :
                    { from : outletPos connector.from
                    , to : inletPos connector.to
                    }
                , handleEvents : handleLinkEvents
                }
                $ FromLink linkR


render HTML ptk state =
    case state.mbCurrentEditor of
        Just (nodeR /\ { inlet, editor, pos, currentValue }) ->
            let
                theInletPos = inletPos (nodeR /\ inlet) # \{x, y} -> { x, y : y - 25.0 }
                _ = Debug.spy "inlet" inlet
                inletPath = { node : nodeR, inlet }
                -- mbWebEditorId = webEditorFor (Proxy :: _ tk) inletPath currentValue
                mbWebEditorComp = spawnWebEditor (Proxy :: _ tk) editor inletPath currentValue
            in
                case mbWebEditorComp of
                    Just valueEditor ->
                        HH.slot _valueEditor editor valueEditor -- TODO: do not spawn a new editor for every new inlet, but replace `send` function inside it?
                            { pos : theInletPos, currentValue : ViC.toFallback currentValue } $ FromValueEditor nodeR inlet
                    Nothing -> HH.div [] []
        Nothing -> HH.div [] []
    where
        nodesToCellsMap = _makeNodesToCellsMap state
        inletPos = _inletPosition nodesToCellsMap


type NodeCell_ sr cr m =
    { inFocus :: Boolean
    , position ::
        { left :: Number
        , top :: Number
        }
    , rawNode :: Raw.Node sr cr m
    , size ::
        { width :: Number
        , height :: Number
        }
    , zIndex :: NodeZIndex
    }


_makeNodesWithCells
    :: forall loc ps sr cr m
     . State loc ps sr cr m
    -> Array (NodeCell_ sr cr m)
_makeNodesWithCells state =
    Debug.spy "nodes with cells" ((Debug.spy "nodes in PA" state.nodes) <#> findCell)
    where
        findCell rawNode =
            let
                nodeR = RawNode.id rawNode
                mbBounds = findBounds nodeR state
                size = fromMaybe bottom $ Bounds.getSize <$> Tuple.fst <$> mbBounds -- FIXME: only the inner `NodeBox` can know the actual size (pass it with query?)
                (position /\ zIndex) =
                    fromMaybe (defaultPosition /\ top)
                         $ lmap Bounds.getPosition
                        <$> mbBounds
            in
                { rawNode, position, zIndex, size, inFocus : Set.member nodeR state.focusedNodes }


_makeNodesToCellsMap
    :: forall loc ps sr cr m
     . State loc ps sr cr m
    -> Map Id.NodeR (NodeCell_ sr cr m)
_makeNodesToCellsMap state =
    Map.fromFoldable $ cellToTuple <$> _makeNodesWithCells state
    where
        cellToTuple cell = RawNode.id cell.rawNode /\ cell


_inletPosition :: forall sr cr m. Map Id.NodeR (NodeCell_ sr cr m) -> (Id.NodeR /\ Id.InletR) -> { x :: Number, y :: Number }
_inletPosition nodesToCellsMap (nodeR /\ inletR) =
    Map.lookup nodeR nodesToCellsMap
    <#> (\{ position, rawNode } ->
        let
            inletIdx = fromMaybe (-1) $ RawShape.indexOfInlet inletR $ RawNode.shape rawNode
            relPos = NodeBox.inletRelPos inletIdx
        in { x : position.left + relPos.x, y : position.top + relPos.y })
    # fromMaybe { x : 0.0, y : 0.0 }


_outletPosition :: forall sr cr m. Map Id.NodeR (NodeCell_ sr cr m) -> (Id.NodeR /\ Id.OutletR) -> { x :: Number, y :: Number }
_outletPosition nodesToCellsMap (nodeR /\ outletR) =
    Map.lookup nodeR nodesToCellsMap
    <#> (\{ position, rawNode } ->
        let
            outletIdx = fromMaybe (-1) $ RawShape.indexOfOutlet outletR $ RawNode.shape rawNode
            relPos = NodeBox.outletRelPos outletIdx
        in { x : position.left + relPos.x, y : position.top + relPos.y })
    # fromMaybe { x : 0.0, y : 0.0 }


handleAction
    :: forall loc ps sr cr m
     . MonadEffect m
    => WebLocator loc
    => Action ps sr cr m
    -> H.HalogenM (State loc ps sr cr m) (Action ps sr cr m) (Slots sr cr) (Output loc cr) m Unit
handleAction = case _ of
    Initialize -> pure unit
    Receive { mbState, offset, size, nodes, nodesBounds, links, zoom, mbCurrentEditor } ->
        H.modify_ _
            { mbState = mbState, offset = offset, size = size, zoom = zoom, nodes = nodes, nodesBounds = nodesBounds, links = links, mbCurrentEditor = mbCurrentEditor }
    PatchAreaMouseMove { x, y } -> do
        state <- H.get
        case state.lockOn of
            DraggingNode nodeR -> do
                H.raise $ MoveNode nodeR { left : x, top : y }
            Connecting linkStart _ -> do
                H.modify_ _ { lockOn = Connecting linkStart { x, y } }
                informLockUpdate
            NoLock ->
                H.modify_ _ { focusedNodes = findFocusedNodes { x, y } state.nodesBounds }
    WheelChange { dy } ->
        H.raise $ TryZoom dy
    PatchAreaClick -> do
        state <- H.get
        whenJust (draggingNode state)
            \nodeR -> do
                H.modify_ _ { lockOn = NoLock }
                H.tell _nodeBox nodeR NodeBox.ApplyDragEnd
                informLockUpdate
        whenJust (creatingLink state)
            \_ -> do
                H.modify_ _ { lockOn = NoLock }
                informLockUpdate
    FromNodeBox nodeR NodeBox.HeaderWasClicked -> do
        state <- H.get
        case (draggingNode state) of -- FIXME: should cancel creating link before starting to drag
            Nothing -> do
                H.modify_ _ { lockOn = DraggingNode nodeR }
                H.tell _nodeBox nodeR NodeBox.ApplyDragStart
                informLockUpdate
            Just otherNodeR -> do
                H.modify_ _ { lockOn = NoLock }
                H.tell _nodeBox otherNodeR NodeBox.ApplyDragEnd
                informLockUpdate
    FromNodeBox nodeR (NodeBox.InletWasClicked inletR) -> do
        state <- H.get
        whenJust (creatingLink state) \{ fromNode, fromOutlet } ->
            when (fromNode /= nodeR) $
                H.raise $ Connect $ { fromNode, fromOutlet } /\ { toNode : nodeR, toInlet : inletR }
        H.modify_ _ { lockOn = NoLock }
        informLockUpdate
        -- TODO ApplyDragEnd if node was dragged
    FromNodeBox nodeR (NodeBox.InletValueWasClicked pos inletR editorId vic) -> do
        let _ = Debug.spy "patch: inlet value click" unit
        let editorDef =
                { inlet : inletR
                , editor : editorId
                , pos
                , currentValue : vic
                }
        H.modify_ _ { mbCurrentEditor = Just $ nodeR /\ editorDef }
        H.raise $ RequestValueEditor nodeR editorDef
    FromNodeBox nodeR (NodeBox.OutletWasClicked outletR pos) -> do
        state <- H.get
        H.modify_ _
            { lockOn = Connecting
                { fromNode : nodeR
                , fromOutlet : outletR
                }
                { x : pos.x - state.offset.left
                , y : pos.y - state.offset.top
                }
            }
        informLockUpdate
    FromNodeBox nodeR (NodeBox.ReportMouseMove mevt) -> do
        state <- H.get
        handleAction $ PatchAreaMouseMove $
            { x : (Int.toNumber $ Mouse.clientX mevt) - state.offset.left
            , y : (Int.toNumber $ Mouse.clientY mevt) - state.offset.top
            }
    FromNodeBox nodeR (NodeBox.UpdateStatusBar tag) -> do
        H.raise $ UpdateStatusBar tag
    FromNodeBox nodeR (NodeBox.ClearStatusBar) -> do
        H.raise $ ClearStatusBar
    FromNodeBox nodeR (NodeBox.RemoveButtonWasClicked) -> do
        H.raise $ RemoveNode nodeR
    PassUpdate nodeR update ->
        H.tell _nodeBox nodeR $ NodeBox.ApplyChanges update
    FromLink linkR (LinkCmp.WasClicked) ->
        H.raise $ Disconnect linkR
    FromValueEditor nodeR inletR (ValueEditor.SendValue value) -> do
        state <- H.get
        whenJust (state.nodes # Array.find (RawNode.id >>> (_ == nodeR)))
            $ RawNode.sendIn inletR (Debug.spy "send value" value)
    FromValueEditor _ _ ValueEditor.CloseEditor -> do
        H.modify_ _ { mbCurrentEditor = Nothing }
        H.raise CloseValueEditor
    where
        informLockUpdate = H.get <#> _.lockOn >>= (H.raise <<< LockUpdate)



handleQuery
    :: forall loc ps sr cr m a
     . MonadEffect m
    => WebLocator loc
    => Query sr cr m a
    -> H.HalogenM (State loc ps sr cr m) (Action ps sr cr m) (Slots sr cr) (Output loc cr) m (Maybe a)
handleQuery = case _ of
    ApplyNewNode rawNode a -> do
        state <- H.get
        let
            nodeRect = { width : 300.0, height : 70.0 }
            nextLoc /\ nodePos = Web.locateNext state.lastLocation nodeRect
        H.modify_ _ { lastLocation = nextLoc }
        H.raise $ NewNodeBounds (RawNode.id rawNode) nextLoc
            { left : nodePos.left, top : nodePos.top
            , width : nodeRect.width, height : nodeRect.height
            }
        pure $ Just a
    ApplyUpdate nodeR update a -> do
        handleAction $ PassUpdate nodeR update
        pure $ Just a
    ValueEditorClosedByUser a -> do
        H.modify_ _ { mbCurrentEditor = Nothing }
        pure $ Just a
    CancelConnecting a -> do
        H.modify_ _ { lockOn = NoLock }
        H.raise $ LockUpdate NoLock
        pure $ Just a


findFocusedNodes :: { x :: Number, y :: Number } -> NodesBounds -> Set Id.NodeR
findFocusedNodes pos = convertMap >>> foldr foldF Set.empty
    where
        convertMap :: NodesBounds -> Array (Id.NodeR /\ (Bounds /\ NodeZIndex))
        convertMap = Map.toUnfoldable
        foldF (nodeR /\ ({ width, height, top, left } /\ _)) set =
            if (pos.x >= left && pos.y >= top && pos.x <= (left + width) && pos.y <= (top + height)) then Set.insert nodeR set else set


draggingNode :: forall loc ps sr cr m. State loc ps sr cr m -> Maybe Id.NodeR
draggingNode = _.lockOn >>> case _ of
    DraggingNode nodeR -> Just nodeR
    Connecting _ _ -> Nothing
    NoLock -> Nothing


creatingLink :: forall loc ps sr cr m. State loc ps sr cr m -> Maybe LinkStart
creatingLink = _.lockOn >>> case _ of
    DraggingNode _ -> Nothing
    Connecting linkStart _ -> Just linkStart
    NoLock -> Nothing


findBounds :: forall loc ps sr cr m. Id.NodeR -> State loc ps sr cr m -> Maybe (Bounds /\ NodeZIndex)
findBounds nodeR = _.nodesBounds >>> Map.lookup nodeR



storeBounds :: Id.NodeR -> Bounds -> NodesBounds -> NodesBounds
storeBounds nodeR bounds nodesBounds =
    nodesBounds
        # Map.insert nodeR
            (bounds /\ (ZIndex $ Map.size nodesBounds))


updatePosition :: Id.NodeR -> { left :: Number, top :: Number } -> NodesBounds -> NodesBounds
updatePosition nodeR { left, top } =
    MapX.update' (lmap $ _ { left = left, top = top }) nodeR