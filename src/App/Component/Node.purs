module App.Component.Node where


import Prelude
import Debug as Debug

import Effect.Class (class MonadEffect, liftEffect)

import Data.Unit (Unit, unit)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Data.Int (toNumber)
import Data.Vec2 as V2
import Data.Vec2 ((<+>))

import Noodle.Node ((+>), (++>))
import Noodle.Node (Node) as Noodle
import Noodle.Node as Node

import App.Style (Style, NodeFlow(..), transparent)
import App.Style.Calculate as Calc
import App.Style.ClassNames as CS

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HSA
import App.UI (UI)
import App.UI as UI

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
        , body
        , name'
        , inlets'
        , outlets'
        ]
    where
        u = style.units flow
        colors = style.colors

        ( slotOuterWidth /\ slotOuterHeight ) = V2.toTuple $ Calc.slotSize u flow
        ( namePlateWidth /\ namePlateHeight ) = V2.toTuple $ Calc.namePlateSize u flow

        ( outerWidth /\ outerHeight ) = node # Calc.nodeBounds u flow # V2.toTuple
        ( innerWidth /\ innerHeight ) = node # Calc.nodeBodySize u flow # V2.toTuple

        inlets = Node.inlets node
        outlets = Node.outlets node

        translateTo pos =
            HSA.transform [ HSA.Translate (V2.x pos) (V2.y pos) ]

        name' =
            HS.g
                [ translateTo $ Calc.namePos u flow
                , HSA.classes $ CS.nodeTitle <> CS.nodeTitleFocus
                ]
                [ HS.rect
                    [ HSA.fill $ Just colors.namePlateBg
                    , HSA.width namePlateWidth
                    , HSA.height namePlateHeight
                    ]
                , HS.g
                    [ translateTo $ Calc.nameTextPos u flow
                    ]
                    [ HS.text
                        [ HSA.fill $ Just colors.nodeName ]
                        [ HH.text name ]
                    ]
                ]

        slot classes rectPos pos textPos (name /\ shape) =
            HS.g
                [ HSA.classes classes ]
                [ HS.g
                    [ translateTo pos ]
                    [ HS.circle
                        [ HSA.fill $ Just colors.slotFill
                        , HSA.stroke $ Just colors.slotStroke
                        , HSA.strokeWidth u.slotStrokeWidth
                        , HSA.r u.slotRadius
                        ]
                    ]
                , HS.g
                    [ translateTo textPos ]
                    [ HS.text
                        [ HSA.fill $ Just colors.slotTextFill ]
                        [ HH.text name ]
                    ]
                , HS.g
                    [ translateTo rectPos
                    , HSA.classes CS.slotFocusArea
                    ]
                    [ HS.rect
                        [ {- HE.onClick
                        , -} HSA.fill $ Just transparent
                        , HSA.width u.slotOuterWidth, HSA.height u.slotOuterHeight
                        ]
                    ]
                ]

        inlets' =
            HS.g [ HSA.classes CS.nodeInlets ]
                $ Array.mapWithIndex inlet' inlets

        inlet' idx (name /\ shape) =
            slot
                (CS.inlet name)
                (Calc.inletRectPos u flow idx)
                (Calc.inletPos u flow idx)
                (Calc.inletTextPos u flow idx)
                (name /\ shape)

        outlets' =
            HS.g [ HSA.classes CS.nodeOutlets ]
                $ Array.mapWithIndex outlet' outlets

        outlet' idx (name /\ shape) =
            slot
                (CS.outlet name)
                (Calc.outletRectPos u flow idx)
                (Calc.outletPos u flow idx)
                (Calc.outletTextPos u flow idx)
                (name /\ shape)

        body =
            HS.g
                [ translateTo $ Calc.bodyPos u flow ]
                [ HS.rect
                    [ HSA.fill $ Just colors.bodyFill
                    , HSA.stroke $ Just colors.bodyStroke
                    , HSA.strokeWidth $ u.bodyStrokeWidth
                    , HSA.rx u.bodyCornerRadius, HSA.ry u.bodyCornerRadius
                    , HSA.width innerWidth, HSA.height innerHeight
                    ]
                ,
                case Node.family node >>= ui.node of
                    Just userNodeBody ->
                        HS.g [ translateTo $ u.bodyPadding <+> u.namePlateHeight ]
                            [ HH.slot _body name userNodeBody node SendData ]
                    Nothing ->
                        HS.g [ ] [ ]
                ]

        shadow =
            HS.g
                [ translateTo $ Calc.shadowPos u flow ]
                [ HS.rect
                    [ HSA.fill $ Just colors.bodyShadow
                    , HSA.stroke $ Just colors.bodyShadow
                    , HSA.strokeWidth $ u.bodyStrokeWidth
                    , HSA.rx u.bodyCornerRadius, HSA.ry u.bodyCornerRadius
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