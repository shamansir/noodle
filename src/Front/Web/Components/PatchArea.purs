module Web.Components.PatchArea where

import Prelude

import Type.Proxy (Proxy(..))

import Effect.Class (class MonadEffect)

import Control.Monad.State (get, put, modify, modify_) as State
import Control.Monad.Extra (whenJust)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (Map)
import Data.Map (empty, lookup, insert, size, fromFoldable) as Map
import Data.Map.Extra (update') as MapX
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (sortWith) as Array
import Data.Int (toNumber) as Int
import Data.Bifunctor (lmap)
import Data.Newtype (unwrap) as NT
import Data.Text.Format (Tag) as T

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.Color as HC
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.Extra as HSX

import Web.UIEvent.MouseEvent (clientX, clientY) as Mouse

import Noodle.Id (NodeR, InletR, OutletR, LinkR) as Id
import Noodle.Raw.Link (Link) as Raw
import Noodle.Raw.Link (id, connector) as RawLink
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (NodeChanges, id, shape) as RawNode
import Noodle.Raw.Fn.Shape as RawShape
import Noodle.Repr.ChRepr (class WriteChannelRepr)
import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette
import Noodle.Ui.Tagging.At (ChannelLabel, StatusLine) as At
import Noodle.Ui.Tagging.At (class At) as T

import Web.Bounds (Bounds)
import Web.Bounds (getPosition) as Bounds
import Web.Components.NodeBox as NodeBox
import Web.Components.Link as LinkCmp
import Web.Class.WebRenderer (class WebLocator, ConstantShift)
import Web.Class.WebRenderer (firstLocation, locateNext) as Web


newtype NodeZIndex = ZIndex Int
derive newtype instance Eq NodeZIndex
derive newtype instance Ord NodeZIndex
instance Bounded NodeZIndex where
    top = ZIndex 1000
    bottom = ZIndex 0


type Locator = ConstantShift -- TODO: move to some root App config?


type Slots sr cr =
    ( nodeBox :: H.Slot (NodeBox.Query sr cr) NodeBox.Output Id.NodeR
    , link :: forall q. H.Slot q LinkCmp.Output Id.LinkR
    )


_nodeBox = Proxy :: _ "nodeBox"
_link = Proxy :: _ "link"


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


type State loc ps sr cr m =
    { offset :: { left :: Number, top :: Number }
    , size :: { width :: Number, height :: Number }
    , state :: ps
    , lastLocation :: loc
    , nodes :: Array (Raw.Node sr cr m)
    , nodesBounds :: Map Id.NodeR (Bounds /\ NodeZIndex)
    , links :: Array Raw.Link
    , lockOn :: LockingTask
    }


type Input ps sr cr m =
    { state :: ps
    , offset :: { left :: Number, top :: Number }
    , size :: { width :: Number, height :: Number }
    , nodes :: Array (Raw.Node sr cr m)
    , links :: Array Raw.Link
    }


data Action ps sr cr m
    = Initialize
    | Receive (Input ps sr cr m)
    | PassUpdate Id.NodeR (RawNode.NodeChanges sr cr)
    | PatchAreaMouseMove { x :: Number, y :: Number }
    | PatchAreaClick
    | FromNodeBox Id.NodeR NodeBox.Output
    | FromLink Id.LinkR LinkCmp.Output


data Output
    = Connect (LinkStart /\ LinkEnd)
    | Disconnect Id.LinkR
    | RemoveNode Id.NodeR
    | UpdateStatusBar T.Tag
    | ClearStatusBar


data Query sr cr m a
    = ApplyNewNode (Raw.Node sr cr m) a
    | ApplyUpdate Id.NodeR (RawNode.NodeChanges sr cr) a


component
    :: forall loc ps sr cr m
     . MonadEffect m
    => WebLocator loc
    => T.At At.StatusLine cr
    => T.At At.ChannelLabel cr
    => Proxy loc
    -> H.Component (Query sr cr m) (Input ps sr cr m) Output m
component ploc =
    H.mkComponent
        { initialState : initialState ploc
        , render : render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            , receive = Just <<< Receive
            , handleQuery = handleQuery
            }
        }


initialState :: forall loc ps sr cr m. WebLocator loc => Proxy loc -> Input ps sr cr m -> State loc ps sr cr m
initialState _ { state, offset, size, nodes, links } =
    { lastLocation : Web.firstLocation
    , state
    , offset
    , size
    , nodes
    , links
    , nodesBounds : Map.empty
    , lockOn : NoLock
    }


render
    :: forall loc ps sr cr m
     . MonadEffect m
    => T.At At.StatusLine cr
    => T.At At.ChannelLabel cr
    => State loc ps sr cr m
    -> H.ComponentHTML (Action ps sr cr m) (Slots sr cr) m
render state =
    HS.g
        []
        [ HS.rect
            [ HSA.width state.size.width, HSA.height state.size.height
            , HSA.fill $ Just $ P.hColorOf $ Palette.black
            , HE.onClick $ const PatchAreaClick
            , HE.onMouseMove \mevt -> PatchAreaMouseMove
                { x : (Int.toNumber $ Mouse.clientX mevt) - state.offset.left
                , y : (Int.toNumber $ Mouse.clientY mevt) - state.offset.top
                }
            ]
        , HS.g
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
    where
        notYetConnectedLink = LinkCmp.linkShapeNotYetConnected
        nodesWithCells = state.nodes <#> findCell
        cellToTuple { rawNode, position, zIndex } = RawNode.id rawNode /\ { rawNode, position, zIndex }
        nodesToCellsMap = Map.fromFoldable $ cellToTuple <$> nodesWithCells
        nodeBoxesSlots = (nodesWithCells # Array.sortWith _.zIndex <#> nodeBoxSlot)
        linksSlots = state.links <#> linkSlot
        findCell rawNode =
            let
                nodeR = RawNode.id rawNode
                (position /\ zIndex) =
                    fromMaybe (defaultPosition /\ top)
                         $ lmap Bounds.getPosition
                        <$> findBounds nodeR state
            in
                { rawNode, position, zIndex }
        nodeBoxSlot { rawNode, position } =
            let
                nodeR = RawNode.id rawNode
            in HH.slot _nodeBox nodeR NodeBox.component
                { node : rawNode
                , position
                }
                $ FromNodeBox nodeR
        inletPos (nodeR /\ inletR) =
            Map.lookup nodeR nodesToCellsMap
            <#> (\{ position, rawNode } ->
                let
                    inletIdx = fromMaybe (-1) $ RawShape.indexOfInlet inletR $ RawNode.shape rawNode
                    relPos = NodeBox.inletRelPos inletIdx
                in { x : position.left + relPos.x, y : position.top + relPos.y })
            # fromMaybe { x : 0.0, y : 0.0 }
        outletPos (nodeR /\ outletR) =
            Map.lookup nodeR nodesToCellsMap
            <#> (\{ position, rawNode } ->
                let
                    outletIdx = fromMaybe (-1) $ RawShape.indexOfOutlet outletR $ RawNode.shape rawNode
                    relPos = NodeBox.outletRelPos outletIdx
                in { x : position.left + relPos.x, y : position.top + relPos.y })
            # fromMaybe { x : 0.0, y : 0.0 }
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
                }
                $ FromLink linkR


handleAction
    :: forall loc ps sr cr m
     . WebLocator loc
    => Action ps sr cr m
    -> H.HalogenM (State loc ps sr cr m) (Action ps sr cr m) (Slots sr cr) Output m Unit
handleAction = case _ of
    Initialize -> pure unit
    Receive { state, offset, nodes, links } ->
        H.modify_ _
            { state = state, offset = offset, nodes = nodes, links = links }
    PatchAreaMouseMove { x, y } -> do
        state <- State.get
        case state.lockOn of
            DraggingNode nodeR ->
                H.modify_ $ updatePosition nodeR { left : x, top : y }
            Connecting linkStart _ ->
                H.modify_ _ { lockOn = Connecting linkStart { x, y } }
            NoLock ->
                pure unit
    PatchAreaClick -> do
        state <- State.get
        whenJust (draggingNode state)
            \nodeR -> do
                State.modify_ _ { lockOn = NoLock }
                H.tell _nodeBox nodeR NodeBox.ApplyDragEnd
    FromNodeBox nodeR NodeBox.HeaderWasClicked -> do
        state <- State.get
        case (draggingNode state) of -- FIXME: should cancel creating link before starting to drag
            Nothing -> do
                State.modify_ _ { lockOn = DraggingNode nodeR }
                H.tell _nodeBox nodeR NodeBox.ApplyDragStart
            Just otherNodeR -> do
                State.modify_ _ { lockOn = NoLock }
                H.tell _nodeBox otherNodeR NodeBox.ApplyDragEnd
    FromNodeBox nodeR (NodeBox.InletWasClicked inletR) -> do
        state <- State.get
        whenJust (creatingLink state) \{ fromNode, fromOutlet } ->
            when (fromNode /= nodeR) $
                H.raise $ Connect $ { fromNode, fromOutlet } /\ { toNode : nodeR, toInlet : inletR }
        State.modify_ _ { lockOn = NoLock }
        -- TODO ApplyDragEnd if node was dragged
    FromNodeBox nodeR (NodeBox.OutletWasClicked outletR pos) -> do
        state <- State.get
        State.modify_ _
            { lockOn = Connecting
                { fromNode : nodeR
                , fromOutlet : outletR
                }
                { x : pos.x - state.offset.left
                , y : pos.y - state.offset.top
                }
            }
    FromNodeBox nodeR (NodeBox.ReportMouseMove mevt) -> do
        state <- State.get
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


handleQuery
    :: forall loc ps sr cr m a
     . WebLocator loc
    => Query sr cr m a
    -> H.HalogenM (State loc ps sr cr m) (Action ps sr cr m) (Slots sr cr) Output m (Maybe a)
handleQuery = case _ of
    ApplyNewNode rawNode a -> do
        state <- State.get
        let
            nodeRect = { width : 300.0, height : 70.0 }
            nextLoc /\ nodePos = Web.locateNext state.lastLocation nodeRect
        H.modify_
            $ _ { lastLocation = nextLoc }
            >>> storeBounds (RawNode.id rawNode)
                { left : nodePos.left, top : nodePos.top
                , width : nodeRect.width, height : nodeRect.height
                }
        pure $ Just a
    ApplyUpdate nodeR update a -> do
        handleAction $ PassUpdate nodeR update
        pure $ Just a


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


storeBounds :: forall loc ps sr cr m. Id.NodeR -> Bounds -> State loc ps sr cr m -> State loc ps sr cr m
storeBounds nodeR bounds s = s
    { nodesBounds
        = s.nodesBounds
            # Map.insert nodeR
                (bounds /\ (ZIndex $ Map.size s.nodesBounds))
    }


updatePosition :: forall loc ps sr cr m. Id.NodeR -> { left :: Number, top :: Number } -> State loc ps sr cr m -> State loc ps sr cr m
updatePosition nodeR { left, top } s = s
    { nodesBounds
        = s.nodesBounds
            # MapX.update' (lmap $ _ { left = left, top = top }) nodeR
    }