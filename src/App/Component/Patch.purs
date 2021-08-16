module App.Component.Patch where


import Prelude

import Debug (spy) as Debug

import Effect.Class (class MonadEffect, liftEffect)

import Data.Array as Array
import Data.BinPack.R2.Optional (Bin2)
import Data.BinPack.R2.Optional as R2
import Data.Int (toNumber, floor)
import Data.Number
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe as Maybe
import Data.PinBoard (PinBoard)
import Data.PinBoard as PB
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec2 (Vec2, Pos, Size, (<+>))
import Data.Vec2 as V2
import Data.Foldable (foldr)

import Control.Alternative ((<|>))

import Type.Proxy (Proxy(..))

import Noodle.Node (Node) as Noodle
import Noodle.Node as Node
import Noodle.Patch (Patch) as Noodle
import Noodle.Patch as Patch
import Noodle.Toolkit (Toolkit) as Noodle
import Noodle.Toolkit as Toolkit
import Noodle.Node.Shape (InletId, OutletId)

import App.Emitters as Emitters
import App.Mouse as Mouse
import App.Style (Style, NodeFlow, Flags)
import App.Style as Style
import App.Style.Calculate as Calc
import App.Style.ClassNames as CS
import App.Svg.Extra (translateTo') as HSA
import App.Toolkit.UI (UI)
import App.Toolkit.UI (flagsFor) as UI

import App.Component.Node as NodeC
import App.Component.ButtonStrip as BS

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HS

import Web.HTML (window)
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent as ME

import App.Component.Link as Link


type Slot id = forall query. H.Slot query Void id


type Slots = ( node :: NodeC.Slot Node.Id, link :: Link.Slot Int )


_node = Proxy :: Proxy "node"


_link = Proxy :: Proxy "link"


data Subject
    = Node Node.Id
    | Inlet (Node.Id /\ InletId)
    | Outlet (Node.Id /\ OutletId)


type Input m d =
    { patch :: Noodle.Patch d
    , toolkit :: Noodle.Toolkit d
    , style :: Style
    , flow :: NodeFlow
    , offset :: Pos
    , ui :: UI m d
    , area :: Size
    -- , bodyRenderer :: Node.Family -> Maybe (UI.NodeComponent m d)
    }


type State m d =
    { patch :: Noodle.Patch d
    , toolkit :: Noodle.Toolkit d
    , style :: Style
    , flow :: NodeFlow
    , offset :: Pos
    , buttonStrip :: BS.ButtonStrip Node.Family
    , layout :: Bin2 Number Node.Id
    , pinned :: PinBoard Node.Id
    , mouse :: Mouse.State (Subject /\ Pos)
    , ui :: UI m d
    , area :: Size
    }


data Action m d
    = Initialize
    | Receive (Input m d)
    | AddNode Node.Family
    | DetachNode Node.Id
    | PinNode Node.Id Pos
    | Connect Patch.OutletPath Patch.InletPath
    | HandleMouse H.SubscriptionId ME.MouseEvent -- TODO Split mouse handing in different actions


initialState :: forall m d. Input m d -> State m d
initialState { patch, toolkit, style, flow, offset, ui, area } =
    { patch, toolkit, style, flow
    , offset : offset
    , layout :
        R2.container area
            # addNodesFrom ui style flow patch
    , buttonStrip : BS.make (V2.w area) $ Toolkit.nodeFamilies toolkit
    , pinned : []
    , mouse : Mouse.init
    , ui, area
    }


render :: forall d m. MonadEffect m => State m d -> H.ComponentHTML (Action m d) Slots m
render state =
    HS.g
        []
        [ mouseState
        , nodeButtons
        , existingLinks
        , nodesLayout
        , pinnedNodes
        , whatIsBeingDragged state.mouse
        ]
    where
        flow = state.flow
        mouseState =
            HS.text
                [ HSA.translateTo' $ 500.0 <+> -20.0
                , HSA.fill $ Just $ Style.white
                ]
                [ HH.text $ show $ state.mouse ]
        assocNode (name /\ pos /\ bounds) =
            state.patch
                # Patch.findNode name
                <#> { name, node : _, x : V2.x pos, y : V2.y pos, w : V2.w bounds, h : V2.h bounds }
        packedNodes'
            = List.catMaybes $ assocNode <$> R2.toList state.layout
        pinnedNodes'
            = Array.catMaybes $ assocNode <$> PB.toArray state.pinned
        nodeButtons
            = HS.g
                [ HSA.classes CS.nodesTabs ]
                $ nodeButton <$> BS.unfold state.buttonStrip
        bsBottomY = V2.h $ BS.size state.buttonStrip
        bsOffset = V2.h' bsBottomY
        --absNodesOffset = bsBottomY
        -- cursorOffset = 0.0 <+> fullButtonHeight
        nodeButton (buttonPos /\ name) =
            HS.g
                [ HSA.classes $ CS.nodeButton name
                , HSA.translateTo' buttonPos
                , HE.onClick \_ -> AddNode name
                ]
                [ HS.rect
                    [ HSA.width $ V2.w BS.buttonSize, HSA.height $ V2.h BS.buttonSize
                    , HSA.fill $ state.ui.markNode name <|> Just state.style.nodeTab.background
                    , HSA.stroke $ Just state.style.nodeTab.stroke
                    , HSA.strokeWidth 1.0
                    ]
                , HS.text [] [ HH.text $ "+ " <> name ]
                ]
        node' { node, name, x, y, w, h } = -- FIXME: use Vec2
            HS.g
                [ HSA.transform [ HSA.Translate x $ bsBottomY + y ]
                , HSA.classes $ CS.node state.flow name
                ]
                [ HH.slot _node name
                    NodeC.component
                        { node, name, style : state.style, flow : state.flow, ui : state.ui }
                    absurd
                ]
        nodesLayout =
            HS.g [ HSA.classes CS.nodes ] $ map node' $ List.toUnfoldable $ packedNodes' -- Patch.nodes patch
        pinnedNodes =
            HS.g [ HSA.classes CS.nodes ] $ map node' $ pinnedNodes'
        floatingNode pos (name /\ nodeOffset) =
            let
                bounds = boundsOf state.ui state.style state.flow state.patch name # Maybe.fromMaybe zero
            in case assocNode ( name /\ (pos - nodeOffset - bsOffset - state.offset) /\ bounds ) of
                Just n -> node' n
                Nothing -> HS.g [] []
        existingLinks =
            HS.g [] $ closedLink <$> (Array.fromFoldable $ Patch.links state.patch)
        linkEndsPositions (srcNodeName /\ outlet) (dstNodeName /\ inlet) =
            (\outletConnectorPos srcNodePos inletConnectorPos dstNodePos ->
                (srcNodePos + outletConnectorPos)
                /\ (dstNodePos + inletConnectorPos)
            )
                <$> (Patch.findNode srcNodeName state.patch
                        >>= NodeC.outletConnectorPos state.ui state.style state.flow outlet)
                <*> findNodePosition srcNodeName
                <*> (Patch.findNode dstNodeName state.patch
                        >>= NodeC.inletConnectorPos state.ui state.style state.flow inlet)
                <*> findNodePosition dstNodeName
        closedLink (outletPath /\ inletPath) =
            case linkEndsPositions outletPath inletPath of
                Just (outletPos /\ inletPos) ->
                    let
                        x1 /\ y1 = V2.toTuple $ bsOffset + outletPos
                        x2 /\ y2 = V2.toTuple $ bsOffset + inletPos
                    in HS.line
                        [ HSA.x1 x1, HSA.x2 x2
                        , HSA.y1 y1, HSA.y2 y2
                        , HSA.strokeWidth 3.0, HSA.stroke $ Just $ Style.white
                        ]
                Nothing -> HS.g [] []
        findNodePosition nodeName =
            (R2.find nodeName state.layout <#> fst)
            <|> (PB.find nodeName state.pinned <#> fst)
        openLink pos (nodeName /\ outlet) =
            case (/\)
                    <$> (Patch.findNode nodeName state.patch
                            >>= NodeC.outletConnectorPos state.ui state.style state.flow outlet
                        )
                    <*> findNodePosition nodeName of
                Just (outletConnectorPos /\ nodePos) ->
                    let
                        x1 /\ y1 = V2.toTuple $ bsOffset + nodePos + outletConnectorPos
                        x2 /\ y2 = V2.toTuple $ pos - state.offset
                    in HS.line
                        [ HSA.x1 x1, HSA.x2 x2
                        , HSA.y1 y1, HSA.y2 y2
                        , HSA.strokeWidth 3.0, HSA.stroke $ Just $ Style.white
                        ]
                Nothing -> HS.g [] []
        whatIsBeingDragged (Mouse.StartDrag pos (Node node /\ offset)) =
            HS.g [] []
            --floatingNode pos (node /\ offset)
        whatIsBeingDragged (Mouse.Dragging _ pos (Node node /\ offset)) =
            floatingNode pos (node /\ offset)
        whatIsBeingDragged (Mouse.Dragging _ pos (Outlet outlet /\ _)) =
            openLink pos outlet
        whatIsBeingDragged _ =
            HS.g [] []


handleAction :: forall output m d. MonadEffect m => Action m d -> H.HalogenM (State m d) (Action m d) Slots output m Unit
handleAction = case _ of

    Initialize -> do
        document <- H.liftEffect $ document =<< window
        H.subscribe' $ Emitters.mouseDown document <<< HandleMouse
        H.subscribe' $ Emitters.mouseMove document <<< HandleMouse
        H.subscribe' $ Emitters.mouseUp document <<< HandleMouse

    Receive input ->
        H.modify_
            (\state ->
                state
                    { area = input.area
                    , layout = state.layout # R2.reflow' input.area
                    , buttonStrip = state.buttonStrip # BS.reflow (V2.w input.area)
                    }
            )

    AddNode name -> do
        toolkit <- H.gets _.toolkit
        maybeNode <- liftEffect $ Toolkit.spawn name toolkit
        case maybeNode of
            Just node -> do
                H.modify_ -- _ { patch = _.patch # Patch.addNode "sum" newNode }
                    (\state ->
                        let nodeName = Patch.addUniqueNodeId state.patch name
                            bounds = NodeC.boundsOf state.ui state.style state.flow node
                        in state
                            { patch = state.patch # Patch.addNode nodeName node
                            , layout = R2.packOne state.layout (R2.item bounds nodeName)
                                        # Maybe.fromMaybe state.layout
                            }
                    )
            Nothing -> pure unit

    DetachNode nodeId ->
        H.modify_ $ \state ->
            state
            { layout =
                state.layout # R2.abandon nodeId
            , pinned =
                state.pinned # PB.unpin nodeId
            }

    PinNode nodeId pos ->
        H.modify_ $ \state ->
        let
            bounds =
                boundsOf state.ui state.style state.flow state.patch nodeId
                    # Maybe.fromMaybe zero
        in
            state
                { pinned =
                    state.pinned # PB.pin (pos - state.offset) bounds nodeId
                }

    Connect outletPath inletPath -> do
        state <- H.get
        nextPatch <- liftEffect $ Patch.connect outletPath inletPath state.patch
        H.modify_ (_ { patch = nextPatch })

    HandleMouse _ mouseEvent -> do
        state <- H.get
        let
            bsOffset = V2.zh $ BS.size $ state.buttonStrip
            mouseOffset = state.offset + bsOffset
            nextMouse
                = state.mouse
                    # Mouse.apply
                            (flip (-) mouseOffset
                            >>> findSubjectUnderPos state
                            )
                    mouseEvent
        H.modify_ (_ { mouse = nextMouse })
        case nextMouse of
            Mouse.StartDrag _ (Node nodeId /\ _) ->
                handleAction $ DetachNode nodeId
            Mouse.DropAt pos (Node nodeId /\ offset) ->
                handleAction $ PinNode nodeId $ pos - bsOffset - offset
            _ ->
                pure unit
        case nextMouse of
            Mouse.DropAt pos (Outlet outlet /\ _) ->
                case findSubjectUnderPos state $ pos - mouseOffset of
                    Just (Inlet inlet /\ _) -> do
                        handleAction $ Connect outlet inlet
                    _ ->
                        pure unit
            _ ->
                pure unit

    where
        liftSubject :: Node.Id -> NodeC.WhereInside -> Subject
        liftSubject nodeId NodeC.Title = Node nodeId
        liftSubject nodeId (NodeC.Inlet inletId) = Inlet $ nodeId /\ inletId
        liftSubject nodeId (NodeC.Outlet outletId) = Outlet $ nodeId /\ outletId
        findSubjectUnderPos :: State m d -> Pos -> Maybe (Subject /\ Pos)
        findSubjectUnderPos state pos =
            (findNodeInLayout state pos <|> findNodeInPinned state pos)
                >>= \(nodeId /\ pos') ->
                        state.patch
                            # Patch.findNode nodeId
                            >>= flip (whereInsideNode state) pos'
                            <#> liftSubject nodeId
                            <#> flip (/\) pos'
        whereInsideNode :: State m d -> Noodle.Node d -> Pos -> Maybe NodeC.WhereInside
        whereInsideNode state =
            NodeC.whereInside state.ui state.style state.flow
        findNodeInLayout state =
            R2.sample state.layout
        findNodeInPinned state =
            flip PB.search state.pinned


component :: forall query output m d. MonadEffect m => H.Component query (Input m d) output m
component =
    H.mkComponent
        { initialState
        , render
        , eval:
            H.mkEval H.defaultEval
                { handleAction = handleAction
                , receive = Just <<< Receive
                , initialize = Just Initialize
                }
        }


addNodesFrom :: forall m d. UI m d -> Style -> NodeFlow -> Noodle.Patch d -> Bin2 Number Node.Id -> Bin2 Number Node.Id
addNodesFrom ui style flow patch layout =
    Patch.nodes patch
        # foldr
            (\(nodeName /\ node) layout' ->
                R2.packOne layout' (R2.item (NodeC.boundsOf ui style flow node) nodeName)
                    # Maybe.fromMaybe layout'
            )
            layout


boundsOf :: forall m d. UI m d -> Style -> NodeFlow -> Noodle.Patch d -> Node.Id -> Maybe Size
boundsOf ui style flow patch nodeId =
    patch
        # Patch.findNode nodeId
        # map (NodeC.boundsOf ui style flow)


instance showSubject :: Show Subject where
    show (Node n) = "node " <> n
    show (Inlet path) = "inlet " <> show path
    show (Outlet path) = "outlet " <> show path
