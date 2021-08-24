module App.Component.Node where


import Prelude
import Debug as Debug

import Effect.Class (class MonadEffect, liftEffect)

import Data.Unit (Unit, unit)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Data.Int (toNumber)
import Data.Vec2 as V2
import Data.Vec2 ((<+>), Pos, Size)

import Control.Alternative ((<|>))

import Noodle.Node ((+>), (++>))
import Noodle.Node (Node) as Noodle
import Noodle.Node as Node
import Noodle.Channel.Shape as Ch
import Noodle.Node.Shape (InletId, OutletId)

import App.Style (Flags, Style, NodeFlow(..), transparent, TitleMode(..))
import App.Style (Flags, defaultFlags) as Style
import App.Style.Calculate as Calc
import App.Style.ClassNames as CS
import Data.Color as Color

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


type Slots d = ( body :: UI.NodeSlot d Node.Id )


data Output
    = Remove
    | Replace Node.Family


_body = Proxy :: Proxy "body"


type Input m d =
    { node :: Noodle.Node d
    , name :: Node.Id
    , style :: Style
    , flow :: NodeFlow
    , ui :: UI m d
    }


type State m d =
    { node :: Noodle.Node d
    , name :: Node.Id
    , style :: Style
    , flow :: NodeFlow
    , ui :: UI m d
    }


data Action m d
    = Receive (Input m d)
    | RequestRemove
    | FromUser (UI.NodeOutput d)
    | NoOp


initialState :: forall m d. Input m d -> State m d
initialState = identity


render :: forall d m. MonadEffect m => State m d -> H.ComponentHTML (Action m d) (Slots d) m
render { node, name, style, flow, ui } =
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

        ( outerWidth /\ outerHeight ) = node # Calc.nodeBounds f style flow # V2.toTuple
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
                                [ HSA.fill $ Just style.title.background
                                , HSA.width titleWidth
                                , HSA.height titleHeight
                                ]
                        OutsideBody ->
                            HS.none
                    , HS.g
                        [ HSA.translateTo' $ Calc.titleTextPos f style flow node
                        ]
                        [ HS.text
                            [ HSA.fill $ (ui.markNode =<< Node.family node) <|> Just style.title.fill ]
                            [ HH.text name ]
                        ]
                    , HS.rect
                        [ HSA.classes $
                            if not debug then CS.nodeTitleFocus else CS.nodeTitleFocusDebug
                        , HSA.fill $ Just transparent
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
                        , HSA.fill $ Just $ Color.rgba 48 48 48 0.0
                        ]
                    , HS.none -- TODO: diag.cross with lines
                    , HS.circle
                        [ HSA.r Calc.removeButtonRadius
                        , HSA.fill $ Just transparent
                        , HE.onClick \_ -> RequestRemove
                        ]
                    ]
            else HS.none

        slot classes rectPos pos textPos (name /\ shape) =
            HS.g
                [ HSA.classes classes ]
                [ HS.g
                    [ HSA.translateTo' pos ]
                    [ HS.circle
                        [ HSA.fill $ ui.markChannel (Ch.id shape) <|> Just style.slot.fill
                        , HSA.stroke $ Just style.slot.stroke
                        , HSA.strokeWidth style.slot.strokeWidth
                        , HSA.r $ fromMaybe 0.0 $ Calc.connectorRadius style.slot.connector
                        ]
                    ]
                , HS.g
                    [ HSA.translateTo' textPos
                    , HSA.classes CS.slotIdLabel
                    ]
                    [ HS.text
                        [ HSA.fill $ Just style.slot.label.color ]
                        [ HH.text name ]
                    ]
                , HS.g
                    [ HSA.translateTo' rectPos
                    , HSA.classes
                        $ if not debug then CS.slotFocusArea else CS.slotFocusAreaDebug
                    ]
                    [ HS.rect
                        [ {- HE.onClick
                        , -} HSA.fill $ Just transparent
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
                (name /\ shape)

        body =
            HS.g
                [ HSA.translateTo' $ Calc.bodyPos f style flow node ]
                [ HS.rect
                    [ HSA.id $ name <> "-body-bg"
                    , HSA.fill $ Just style.body.fill
                    , HSA.stroke $ Just style.body.stroke
                    , HSA.strokeWidth $ style.body.strokeWidth
                    , HSA.rx style.body.cornerRadius, HSA.ry style.body.cornerRadius
                    , HSA.width innerWidth, HSA.height innerHeight
                    ]
                ,
                case Node.family node >>= ui.node of
                    Just userNodeBody ->
                        HS.g
                            [ HSA.translateTo' $ Calc.bodyInnerOffset f style flow node ]
                            [ HH.slot _body name userNodeBody { node } FromUser ]
                    Nothing ->
                        HS.none
                ]

        shadow =
            HS.g
                [ HSA.translateTo' $ Calc.shadowPos f style flow node ]
                [ HS.rect
                    [ HSA.fill $ Just $ Color.named "black" -- style.body.shadow
                    , HSA.stroke $ Just $ Color.named "black"
                    , HSA.strokeWidth style.body.strokeWidth
                    , HSA.rx style.body.cornerRadius, HSA.ry style.body.cornerRadius
                    , HSA.width innerWidth, HSA.height innerHeight
                    ]
                ]


handleAction :: forall m d. MonadEffect m => Action m d -> H.HalogenM (State m d) (Action m d) (Slots d) Output m Unit
handleAction = case _ of
    Receive input ->
        H.modify_ (\state -> state { node = input.node })

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

    NoOp ->
        pure unit


component :: forall query m d. MonadEffect m => H.Component query (Input m d) Output m
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



whereInside :: forall m d. UI m d -> Style -> NodeFlow -> Noodle.Node d -> Pos -> Maybe WhereInside
whereInside ui style flow node pos =
    if V2.inside'
        (pos - Calc.titlePos f style flow node)
        (Calc.titleSize f style flow node) then
        Just Header
    else
        let inlets = Node.inlets node <#> fst # Array.mapWithIndex (/\)
            outlets = Node.outlets node <#> fst # Array.mapWithIndex (/\)
            isInSlot sl fn (idx /\ slotName) =
                if V2.inside pos (fn idx /\ Calc.slotArea f style flow node)
                    then Just $ sl slotName
                    else Nothing
            testInlets = Array.findMap (isInSlot Inlet $ Calc.inletRectPos f style flow node) inlets
            testOutlets = Array.findMap (isInSlot Outlet $ Calc.outletRectPos f style flow node) outlets
        in testOutlets <|> testInlets
    where
        f = UI.flagsFor ui node


boundsOf :: forall m d. UI m d -> Style -> NodeFlow -> Noodle.Node d -> Size
boundsOf ui style flow node =
    Calc.nodeBounds (UI.flagsFor ui node) style flow node


inletConnectorPos :: forall m d. UI m d -> Style -> NodeFlow -> InletId -> Noodle.Node d -> Maybe Pos
inletConnectorPos ui style flow inletId node =
    Node.indexOfInlet inletId node
        <#> Calc.inletConnectorPos
                (UI.flagsFor ui node)
                style
                flow
                node


outletConnectorPos :: forall m d. UI m d -> Style -> NodeFlow -> OutletId -> Noodle.Node d -> Maybe Pos
outletConnectorPos ui style flow outletId node =
    Node.indexOfOutlet outletId node
        <#> Calc.outletConnectorPos
                (UI.flagsFor ui node)
                style
                flow
                node
