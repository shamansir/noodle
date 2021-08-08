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

import App.Style (Flags, Style, NodeFlow(..), Units, transparent)
import App.Style (Flags, defaultFlags) as Style
import App.Style.Calculate as Calc
import App.Style.ClassNames as CS

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HSA

import App.Toolkit.UI (UI)
import App.Toolkit.UI as UI
import App.Svg.Extra (translateTo') as HSA
import App.Style (Side)

import Type.Proxy (Proxy(..))


type Slot id = forall query. H.Slot query Void id


type Slots d = ( body :: UI.NodeSlot d Node.Id )


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
    | SendData (UI.NodeOutput d)
    | NoOp


initialState :: forall m d. Input m d -> State m d
initialState = identity


render :: forall d m. MonadEffect m => State m d -> H.ComponentHTML (Action m d) (Slots d) m
render { node, name, style, flow, ui } =
    HS.g
        []
        [ shadow
        , name'
        , inlets'
        , outlets'
        ]
    where
        flagsFor :: Noodle.Node d -> Style.Flags
        flagsFor = UI.flagsFor ui

        f = flagsFor node
        u = style.units flow
        colors = style.colors
        dir = style.slot.direction

        ( slotOuterWidth /\ slotOuterHeight ) = V2.toTuple $ Calc.slotArea f u flow
        ( namePlateWidth /\ namePlateHeight ) = V2.toTuple $ Calc.titleSize f u flow

        ( outerWidth /\ outerHeight ) = node # Calc.nodeBounds f u flow # V2.toTuple
        ( innerWidth /\ innerHeight ) = node # Calc.bodySize f u flow # V2.toTuple

        fitSize :: V2.Size_ Side -> V2.Size -> V2.Size
        fitSize _ a = a

        inlets = Node.inlets node
        outlets = Node.outlets node

        name' =
            HS.g
                [ HSA.translateTo' $ Calc.titlePos f u flow
                , HSA.classes $ CS.nodeTitle <> CS.nodeTitleFocus
                ]
                [ HS.rect
                    [ HSA.fill $ Just colors.title.background
                    , HSA.width namePlateWidth
                    , HSA.height namePlateHeight
                    ]
                , HS.g
                    [ HSA.translateTo' $ Calc.titleTextPos f u flow
                    ]
                    [ HS.text
                        [ HSA.fill $ (ui.markNode =<< Node.family node) <|> Just colors.title.fill ]
                        [ HH.text name ]
                    ]
                ]

        slot classes rectPos pos textPos (name /\ shape) =
            HS.g
                [ HSA.classes classes ]
                [ HS.g
                    [ HSA.translateTo' pos ]
                    [ HS.circle
                        [ HSA.fill $ ui.markChannel (Ch.id shape) <|> Just colors.slot.fill
                        , HSA.stroke $ Just colors.slot.stroke
                        , HSA.strokeWidth u.slot.strokeWidth
                        , HSA.r u.slot.radius
                        ]
                    ]
                , HS.g
                    [ HSA.translateTo' textPos ]
                    [ HS.text
                        [ HSA.fill $ Just colors.slot.label ]
                        [ HH.text name ]
                    ]
                , HS.g
                    [ HSA.translateTo' rectPos
                    , HSA.classes CS.slotFocusArea
                    ]
                    [ HS.rect
                        [ {- HE.onClick
                        , -} HSA.fill $ Just transparent
                        , HSA.width $ V2.w $ u.slot.area
                        , HSA.height $ V2.h $ u.slot.area
                        ]
                    ]
                ]

        inlets' =
            HS.g [ HSA.classes CS.nodeInlets ]
                $ Array.mapWithIndex inlet' inlets

        inlet' idx (name /\ shape) =
            slot
                (CS.inlet name)
                (Calc.inletRectPos f u flow idx)
                (Calc.inletConnectorPos dir f u flow idx)
                (Calc.inletTextPos dir f u flow idx)
                (name /\ shape)

        outlets' =
            HS.g [ HSA.classes CS.nodeOutlets ]
                $ Array.mapWithIndex outlet' outlets

        outlet' idx (name /\ shape) =
            slot
                (CS.outlet name)
                (Calc.outletRectPos f u flow idx)
                (Calc.outletConnectorPos dir f u flow idx)
                (Calc.outletTextPos dir f u flow idx)
                (name /\ shape)

        body =
            HS.g
                [ HSA.translateTo' $ Calc.bodyPos f u flow ]
                [ HS.rect
                    [ HSA.fill $ Just colors.body.fill
                    , HSA.stroke $ Just colors.body.stroke
                    , HSA.strokeWidth $ u.body.strokeWidth
                    , HSA.rx u.body.cornerRadius, HSA.ry u.body.cornerRadius
                    , HSA.width innerWidth, HSA.height innerHeight
                    ]
                ,
                case Node.family node >>= ui.node of
                    Just userNodeBody ->
                        HS.g
                            [ HSA.translateTo' $ V2.h u.title.padding <+> V2.h (fitSize u.title.size V2.zero) ]
                            [ HH.slot _body name userNodeBody node SendData ]
                    Nothing ->
                        HS.g [ ] [ ]
                ]

        shadow =
            HS.g
                [ HSA.translateTo' $ Calc.shadowPos f u flow ]
                [ HS.rect
                    [ HSA.fill $ Just colors.body.shadow
                    , HSA.stroke $ Just colors.body.shadow
                    , HSA.strokeWidth $ u.body.strokeWidth
                    , HSA.rx u.body.cornerRadius, HSA.ry u.body.cornerRadius
                    , HSA.width innerWidth, HSA.height innerHeight
                    ]
                ]


handleAction :: forall output m d. MonadEffect m => Action m d -> H.HalogenM (State m d) (Action m d) (Slots d) output m Unit
handleAction = case _ of
    Receive input ->
        H.modify_ (\state -> state { node = input.node })

    SendData (UI.SendToOutlet outlet d) -> do
        state <- H.get
        liftEffect (state.node ++> (outlet /\ d))

    SendData (UI.SendToInlet inlet d) -> do
        state <- H.get
        liftEffect (state.node +> (inlet /\ d))

    NoOp ->
        pure unit


component :: forall query output m d. MonadEffect m => H.Component query (Input m d) output m
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
    = Title
    | Inlet InletId
    | Outlet OutletId



whereInside :: forall m d. UI m d -> Style -> NodeFlow -> Noodle.Node d -> Pos -> Maybe WhereInside
whereInside ui style flow node pos =
    if V2.inside'
        (pos - Calc.titlePos f u flow)
        (Calc.titleSize f u flow) then
        Just Title
    else
        let inlets = Node.inlets node <#> fst # Array.mapWithIndex (/\)
            outlets = Node.outlets node <#> fst # Array.mapWithIndex (/\)
            isInSlot sl fn (idx /\ slotName) =
                if V2.inside pos (fn idx /\ Calc.slotArea f u flow)
                    then Just $ sl slotName
                    else Nothing
            testInlets = Array.findMap (isInSlot Inlet $ Calc.inletRectPos f u flow) inlets
            testOutlets = Array.findMap (isInSlot Outlet $ Calc.outletRectPos f u flow) outlets
        in testOutlets <|> testInlets
    where
        f = UI.flagsFor ui node
        u = style.units flow


boundsOf :: forall m d. UI m d -> Style -> NodeFlow -> Noodle.Node d -> Size
boundsOf ui style flow node =
    Calc.nodeBounds (UI.flagsFor ui node) (style.units flow) flow node


inletConnectorPos :: forall m d. UI m d -> Style -> NodeFlow -> InletId -> Noodle.Node d -> Maybe Pos
inletConnectorPos ui style flow inletId node =
    Node.indexOfInlet inletId node
        <#> Calc.inletConnectorPos
                style.slot.direction
                (UI.flagsFor ui node)
                (style.units flow)
                flow


outletConnectorPos :: forall m d. UI m d -> Style -> NodeFlow -> OutletId -> Noodle.Node d -> Maybe Pos
outletConnectorPos ui style flow outletId node =
    Node.indexOfOutlet outletId node
        <#> Calc.outletConnectorPos
                style.slot.direction
                (UI.flagsFor ui node)
                (style.units flow)
                flow
