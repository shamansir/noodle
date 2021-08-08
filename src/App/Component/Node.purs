module App.Component.Node where


import Prelude
import Debug as Debug

import Effect.Class (class MonadEffect, liftEffect)

import Data.Unit (Unit, unit)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Data.Int (toNumber)
import Data.Vec2 as V2
import Data.Vec2 ((<+>))
import Control.Alternative ((<|>))

import Noodle.Node ((+>), (++>))
import Noodle.Node (Node) as Noodle
import Noodle.Node as Node
import Noodle.Channel.Shape as Ch

import App.Style (Style, NodeFlow(..), transparent)
import App.Style (Flags, defaultFlags) as Style
import App.Style.Calculate as Calc
import App.Style.ClassNames as CS

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HSA

import App.UI (UI)
import App.UI as UI
import App.Svg.Extra (translateTo') as HSA

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
        flagsFor :: Noodle.Node d -> Style.Flags
        flagsFor node = fromMaybe Style.defaultFlags $ ui.flags <$> Node.family node

        f = flagsFor node
        u = style.units flow
        colors = style.colors
        dir = style.slot.direction

        ( slotOuterWidth /\ slotOuterHeight ) = V2.toTuple $ Calc.slotArea f u flow
        ( namePlateWidth /\ namePlateHeight ) = V2.toTuple $ Calc.titleSize f u flow

        ( outerWidth /\ outerHeight ) = node # Calc.nodeBounds f u flow # V2.toTuple
        ( innerWidth /\ innerHeight ) = node # Calc.nodeBodySize f u flow # V2.toTuple

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
                        , HSA.width $ V2.w $ u.slot.outerSize
                        , HSA.height $ V2.h $ u.slot.outerSize
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
                    , HSA.strokeWidth $ u.nodeBody.strokeWidth
                    , HSA.rx u.nodeBody.cornerRadius, HSA.ry u.nodeBody.cornerRadius
                    , HSA.width innerWidth, HSA.height innerHeight
                    ]
                ,
                case Node.family node >>= ui.node of
                    Just userNodeBody ->
                        HS.g
                            [ HSA.translateTo' $ u.title.padding <+> V2.h u.title.size ]
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
                    , HSA.strokeWidth $ u.nodeBody.strokeWidth
                    , HSA.rx u.nodeBody.cornerRadius, HSA.ry u.nodeBody.cornerRadius
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