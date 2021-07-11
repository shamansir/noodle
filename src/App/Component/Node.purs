module App.Component.Node where


import Prelude

import Data.Unit (Unit, unit)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Data.Int (toNumber)

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

import Type.Proxy (Proxy(..))


type Slot id = forall query. H.Slot query Void id


type Input d =
    { node :: Noodle.Node d
    , name :: String
    , style :: Style
    , flow :: NodeFlow
    }


type State d =
    { node :: Noodle.Node d
    , name :: String
    , style :: Style
    , flow :: NodeFlow
    }


data Action d
    = Receive (Input d)


initialState :: forall d. Input d -> State d
initialState = identity


render :: forall d m. State d -> H.ComponentHTML (Action d) () m
render { node, name, style, flow } =
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

        ( slotOuterWidth /\ slotOuterHeight ) = Calc.slotSize u flow
        ( namePlateWidth /\ namePlateHeight ) = Calc.namePlateSize u flow

        ( outerWidth /\ outerHeight ) = node # Calc.nodeBounds u flow
        ( innerWidth /\ innerHeight ) = node # Calc.nodeBodySize u flow

        inlets = Node.inlets node
        outlets = Node.outlets node

        translateTo (x /\ y) =
            HSA.transform [ HSA.Translate x y ]

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


handleAction :: forall output m d. Action d -> H.HalogenM (State d) (Action d) () output m Unit
handleAction = case _ of
  Receive input ->
    H.modify_ (\state -> state { node = input.node })


component :: forall query output m d. H.Component query (Input d) output m
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