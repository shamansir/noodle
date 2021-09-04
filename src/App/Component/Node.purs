module App.Component.Node where


import Prelude
import Debug as Debug

import Effect.Class (class MonadEffect, liftEffect)

import Color as C
import Color.Extra as C

import Data.Unit (Unit, unit)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Data.Int (toNumber)
import Data.Vec2 as V2
import Data.Vec2 ((<+>), Pos, Size)
import Data.Map as Map
import Data.Map.Extra (type (/->))

import Control.Alternative ((<|>))

import Noodle.Node ((+>), (++>))
import Noodle.Node (Node) as Noodle
import Noodle.Node as Node
import Noodle.Channel.Shape as Ch
import Noodle.Node.Shape (InletId, OutletId)

import App.Style (Style, NodeFlow(..), TitleMode(..), Connector(..))
import App.Style (Flags, defaultFlags) as Style
import App.Style.Calculate as Calc
import App.Style.ClassNames as CS

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.None as HS
import Halogen.Svg.Attributes as HSA

import App.Toolkit.UI (UI)
import App.Toolkit.UI as UI
import App.Svg.Extra (translateTo') as HSA

import Type.Proxy (Proxy(..))


debug :: Boolean
debug = false


type Slot id = forall query. H.Slot query Output id


type Slots s d = ( body :: UI.NodeSlot s d Node.Id )


data Output
    = Remove
    | Replace Node.Family


_body = Proxy :: Proxy "body"


type Input m s d =
    { node :: Noodle.Node d
    , name :: Node.Id
    , style :: Style
    , flow :: NodeFlow
    , ui :: UI m s d
    , linksCount :: Node.LinksCount
    }


type State m s d =
    { node :: Noodle.Node d
    , name :: Node.Id
    , style :: Style
    , flow :: NodeFlow
    , ui :: UI m s d
    , linksCount :: Node.LinksCount
    }


data Action m s d
    = Receive (Input m s d)
    | RequestRemove
    | FromUser (UI.NodeOutput s d)
    | NoOp


initialState :: forall m s d. Input m s d -> State m s d
initialState = identity


render :: forall m s d. MonadEffect m => State m s d -> H.ComponentHTML (Action m s d) (Slots s d) m
render { node, name, style, flow, ui, linksCount } =
    HS.g
        []
        [ shadow
        , body
        , name'
        , removeButton'
        , inlets'
        , outlets'
        ]
    where
        flagsFor :: Noodle.Node d -> Style.Flags
        flagsFor = UI.flagsFor ui

        f = flagsFor node

        ( slotAreaWidth /\ slotAreaHeight ) = V2.toTuple $ Calc.slotArea f style flow node
        ( titleWidth /\ titleHeight ) = V2.toTuple $ Calc.titleSize f style flow node

        ( outerWidth /\ outerHeight ) = node # Calc.nodeArea f style flow # V2.toTuple
        ( innerWidth /\ innerHeight ) = node # Calc.bodySize f style flow # V2.toTuple

        inlets = Node.inletsBy (not Ch.isHidden) node
        outlets = Node.outletsBy (not Ch.isHidden) node

        name' =
            if f.hasTitle then
                HS.g
                    [ HSA.translateTo' $ Calc.titlePos f style flow node
                    , HSA.classes $ CS.nodeTitle
                    ]
                    [ case style.title.mode of
                        InsideBody ->
                             HS.rect
                                [ HSA.fill $ Just $ C.toSvg style.title.background
                                , HSA.width titleWidth
                                , HSA.height titleHeight
                                ]
                        OutsideBody ->
                            HS.none
                    , HS.g
                        [ HSA.translateTo' $ Calc.titleTextPos f style flow node
                        ]
                        [ HS.text
                            [ HSA.fill $ C.toSvg <$> ((ui.markNode =<< Node.family node) <|> Just style.title.fill) ]
                            [ HH.text name ]
                        ]
                    , HS.rect
                        [ HSA.classes $
                            if not debug then CS.nodeTitleFocus else CS.nodeTitleFocusDebug
                        , HSA.fill $ Just $ C.toSvg C.transparent
                        , HSA.width titleWidth
                        , HSA.height titleHeight
                        ]
                    ]
            else HS.none

        removeButton' =
            if f.hasRemoveButton then
                HS.g
                    [ HSA.translateTo' $ Calc.removeButtonPos f style flow node
                    , HSA.classes $ CS.removeButton
                    ]
                    [ HS.circle
                        [ HSA.r Calc.removeButtonRadius
                        , HSA.fill $ Just $ C.toSvg $ C.rgba 48 48 48 0.0
                        ]
                    , HS.line
                        [ HSA.x1 $ -2.5, HSA.y1 $ -2.5, HSA.x2 2.5, HSA.y2 2.5
                        , HSA.stroke $ Just $ C.toSvg $ C.white -- C.rgb 200 0 0
                        , HSA.strokeWidth 1.0
                        ]
                    , HS.line
                        [ HSA.x1 $ -2.5, HSA.y1 2.5, HSA.x2 2.5, HSA.y2 $ -2.5
                        , HSA.stroke $ Just $ C.toSvg $ C.white -- C.rgb 200 0 0
                        , HSA.strokeWidth 1.0
                        ]
                    , HS.circle
                        [ HSA.r Calc.removeButtonRadius
                        , HSA.fill $ Just $ C.toSvg C.transparent
                        , HE.onClick \_ -> RequestRemove
                        ]
                    ]
            else HS.none

        dimAmount = 0.3
        dimColors linksCount = style.slot.dimWhenNoLinks && linksCount == 0

        slotFill lC shape =
            if not $ dimColors lC then
                ui.markChannel (Ch.id shape) <|> Just style.slot.fill
            else
                C.dim dimAmount <$> ui.markChannel (Ch.id shape) <|> Just style.slot.fill

        connector (Circle r) lC shape =
            HS.circle
                [ HSA.fill $ C.toSvg <$> slotFill lC shape
                , HSA.stroke $ Just $ C.toSvg style.slot.stroke
                , HSA.strokeWidth style.slot.strokeWidth
                , HSA.r r
                ]

        connector (DoubleCircle ir or) lC shape =
            HS.g
                []
                [ if lC > 0 then
                    HS.circle
                        [ HSA.fill $ C.toSvg <$> slotFill lC shape
                        , HSA.strokeWidth 0.0
                        , HSA.r ir
                        ]
                  else HS.none
                , HS.circle
                    [ HSA.fill Nothing
                    , HSA.stroke $ C.toSvg <$> slotFill lC shape
                    , HSA.strokeWidth style.slot.strokeWidth
                    , HSA.r or
                    ]
                ]

        connector (Rect s) lC shape =
            HS.rect
                [ HSA.fill $ C.toSvg <$> slotFill lC shape
                , HSA.stroke $ Just $ C.toSvg style.slot.stroke
                , HSA.strokeWidth style.slot.strokeWidth
                , HSA.width $ V2.w s
                , HSA.height $ V2.h s
                ]

        connector (Square n) lC shape =
            HS.rect
                [ HSA.fill $ C.toSvg <$> slotFill lC shape
                , HSA.stroke $ Just $ C.toSvg style.slot.stroke
                , HSA.strokeWidth style.slot.strokeWidth
                , HSA.width n
                , HSA.height n
                ]

        slot classes rectPos pos textPos linksCount (name /\ shape) =
            HS.g
                [ HSA.classes classes ]
                [ HS.g
                    [ HSA.translateTo' pos ]
                    [ connector style.slot.connector linksCount shape
                    ]
                , HS.g
                    [ HSA.translateTo' textPos
                    , HSA.classes CS.slotIdLabel
                    ]
                    [ HS.text
                        [ HSA.fill $ Just $
                            if not $ dimColors linksCount
                                then C.toSvg style.slot.label.color
                                else C.toSvg $ C.dim dimAmount $ style.slot.label.color
                        ]
                        [ HH.text name ]
                    ]
                , HS.g
                    [ HSA.translateTo' rectPos
                    , HSA.classes
                        $ if not debug then CS.slotFocusArea else CS.slotFocusAreaDebug
                    ]
                    [ HS.rect
                        [ {- HE.onClick
                        , -} HSA.fill $ Just $ C.toSvg C.transparent
                        , HSA.width slotAreaWidth
                        , HSA.height slotAreaHeight
                        ]
                    ]
                ]

        inlets' =
            HS.g [ HSA.classes $ CS.nodeInlets style.slot.direction ]
                $ Array.mapWithIndex inlet' inlets

        inlet' idx (name /\ shape) =
            slot
                (CS.inlet name)
                (Calc.inletRectPos f style flow node idx)
                (Calc.inletConnectorPos f style flow node idx)
                (Calc.inletTextPos f style flow node idx)
                (Node.linksAtInlet name linksCount)
                (name /\ shape)

        outlets' =
            HS.g [ HSA.classes $ CS.nodeOutlets style.slot.direction ]
                $ Array.mapWithIndex outlet' outlets

        outlet' idx (name /\ shape) =
            slot
                (CS.outlet name)
                (Calc.outletRectPos f style flow node idx)
                (Calc.outletConnectorPos f style flow node idx)
                (Calc.outletTextPos f style flow node idx)
                (Node.linksAtOutlet name linksCount)
                (name /\ shape)

        body =
            HS.g
                [ HSA.translateTo' $ Calc.bodyPos f style flow node ]
                [ case Node.family node >>= ui.node of
                    Just _ ->
                        HS.mask
                            [ HSA.id $ name <> "-body-mask"
                            ]
                            [ HS.rect
                                [ HSA.fill $ Just $ C.toSvg $ C.black
                                , HSA.width innerWidth, HSA.height innerHeight
                                ]
                            , HS.rect
                                [ HSA.fill $ Just $ C.toSvg $ C.white
                                , HSA.stroke $ Just $ C.toSvg $ C.black
                                , HSA.strokeWidth $ style.body.strokeWidth
                                , HSA.rx style.body.cornerRadius, HSA.ry style.body.cornerRadius
                                , HSA.width innerWidth, HSA.height innerHeight
                                ]
                            ]
                    Nothing -> HS.none
                , HS.rect
                    [ HSA.id $ name <> "-body-bg"
                    , HSA.fill $ Just $ C.toSvg style.body.fill
                    , HSA.stroke $ Just $ C.toSvg style.body.stroke
                    , HSA.strokeWidth $ style.body.strokeWidth
                    , HSA.rx style.body.cornerRadius, HSA.ry style.body.cornerRadius
                    , HSA.width innerWidth, HSA.height innerHeight
                    ]
                , case Node.family node >>= ui.node of
                    Just userNodeBody ->
                        HS.g
                            [ HSA.translateTo' $ Calc.bodyInnerOffset f style flow node
                            , HSA.mask $ "url(#" <> name <> "-body-mask)"
                            ]
                            [ HH.slot _body name userNodeBody { node, state : ui.state } FromUser ]
                    Nothing ->
                        HS.none
                ]

        shadow =
            HS.g
                [ HSA.translateTo' $ Calc.shadowPos f style flow node ]
                [ HS.rect
                    [ HSA.fill $ Just $ C.toSvg $ C.black -- style.body.shadow
                    , HSA.stroke $ Just $ C.toSvg $ C.black
                    , HSA.strokeWidth style.body.strokeWidth
                    , HSA.rx style.body.cornerRadius, HSA.ry style.body.cornerRadius
                    , HSA.width innerWidth, HSA.height innerHeight
                    ]
                ]


handleAction :: forall m s d. MonadEffect m => Action m s d -> H.HalogenM (State m s d) (Action m s d) (Slots s d) Output m Unit
handleAction = case _ of
    Receive input ->
        H.modify_ (\state -> state { node = input.node, linksCount = input.linksCount })

    FromUser (UI.SendToOutlet outlet d) -> do
        state <- H.get
        liftEffect (state.node ++> (outlet /\ d))

    FromUser (UI.SendToInlet inlet d) -> do
        state <- H.get
        liftEffect (state.node +> (inlet /\ d))

    RequestRemove -> do
        H.raise Remove

    FromUser (UI.Replace family) -> do
        H.raise $ Replace family

    FromUser (UI.Update userState) ->
        pure unit -- TODO

    NoOp ->
        pure unit


component :: forall query m s d. MonadEffect m => H.Component query (Input m s d) Output m
component =
    H.mkComponent
        { initialState
        , render
        , eval:
            H.mkEval H.defaultEval
                { handleAction = handleAction
                , receive = Just <<< Receive
                }
        }


data WhereInside
    = Header
    | Inlet InletId
    | Outlet OutletId


whereInside :: forall m s d. UI m s d -> Style -> NodeFlow -> Noodle.Node d -> Pos -> Maybe WhereInside
whereInside ui style flow node pos =
    if V2.inside'
        (pos - Calc.titlePos f style flow node)
        (Calc.titleSize f style flow node) then
        Just Header
    else
        let inlets = Node.inletsBy (not Ch.isHidden) node <#> Tuple.fst # Array.mapWithIndex (/\)
            outlets = Node.outletsBy (not Ch.isHidden) node <#> Tuple.fst # Array.mapWithIndex (/\)
            isInSlot sl fn (idx /\ slotName) =
                if V2.inside pos (fn idx /\ Calc.slotArea f style flow node)
                    then Just $ sl slotName
                    else Nothing
            testInlets = Array.findMap (isInSlot Inlet $ Calc.inletRectPos f style flow node) inlets
            testOutlets = Array.findMap (isInSlot Outlet $ Calc.outletRectPos f style flow node) outlets
        in testOutlets <|> testInlets
    where
        f = UI.flagsFor ui node


areaOf :: forall m s d. UI m s d -> Style -> NodeFlow -> Noodle.Node d -> Size
areaOf ui style flow node =
    Calc.nodeArea (UI.flagsFor ui node) style flow node


inletConnectorPos :: forall m s d. UI m s d -> Style -> NodeFlow -> InletId -> Noodle.Node d -> Maybe Pos
inletConnectorPos ui style flow inletId node =
    Node.indexOfInlet inletId node
        <#> Calc.inletConnectorPos
                (UI.flagsFor ui node)
                style
                flow
                node


outletConnectorPos :: forall m s d. UI m s d -> Style -> NodeFlow -> OutletId -> Noodle.Node d -> Maybe Pos
outletConnectorPos ui style flow outletId node =
    Node.indexOfOutlet outletId node
        <#> Calc.outletConnectorPos
                (UI.flagsFor ui node)
                style
                flow
                node