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

import App.Colors as Colors
import App.ClassNames as CS
import App.Units as U

import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HSA

import Type.Proxy (Proxy(..))


data Direction
    = Vertical
    | Horizontal


direction = Vertical


type Slot id = forall query. H.Slot query Void id


type Input d =
    { node :: Noodle.Node d
    , name :: String
    }


type State d =
    { node :: Noodle.Node d
    , name :: String
    }


data Action d
    = Receive (Input d)


initialState :: forall d. Input d -> State d
initialState = identity


render :: forall d m. State d -> H.ComponentHTML (Action d) () m
render { node, name } =
    HS.g
        []
        [ shadow
        , body
        , inlets'
        , outlets'
        , name'
        ]
    where

        inlets = Node.inlets node
        outlets = Node.outlets node

        inletPos Vertical idx =
            U.slotOuterWidth /\ (U.slotOuterHeight * toNumber idx)
        inletPos Horizontal idx =
            0.0 /\ toNumber idx
        outletPos Vertical idx =
            ( U.slotOuterWidth + U.nodeBodyWidth) /\ U.slotOuterHeight * toNumber idx
        outletPos Horizontal idx =
            0.0 /\ toNumber idx
        inletRectPos Vertical idx =
            ((U.slotOuterWidth - U.slotRadius / 2.0) /\ (U.slotOuterHeight * toNumber idx))
        inletRectPos Horizontal idx =
            0.0 /\ toNumber idx
        outletRectPos Vertical idx =
            (U.nodeBodyWidth - U.slotOuterWidth) /\ U.slotOuterHeight * toNumber idx
        outletRectPos Horizontal idx =
            0.0 /\ toNumber idx
        bodyPos Vertical = U.slotOuterWidth /\ 0.0
        bodyPos Horizontal = U.slotOuterWidth /\ 0.0
        inletTextPos Vertical idx =
            case inletPos Vertical idx of
                x /\ y -> (x - U.slotOuterWidth) /\ y
        inletTextPos Horizontal idx = 0.0 /\ 0.0
        outletTextPos Vertical idx =
            case outletPos Vertical idx of
                x /\ y -> (x + U.slotRadius + 5.0) /\ y
        outletTextPos Horizontal idx = 0.0 /\ 0.0
        shadowPos dir = case bodyPos dir of
            x /\ y -> (x + U.bodyShadowShift) /\ (y + U.bodyShadowShift)
        namePos Vertical =
            (U.slotOuterWidth + 5.0) /\ 0.0
        namePos Horizontal = 0.0 /\ 0.0

        ( outerWidth /\ outerHeight ) = findBounds node
        bodySize Vertical = U.nodeBodyWidth /\ outerHeight
        bodySize Horizontal = U.nodeBodyWidth /\ U.nodeBodyHeight
        ( innerWidth /\ innerHeight ) = bodySize direction
        slotSize Vertical = U.slotOuterWidth /\ U.slotOuterHeight
        slotSize Horizontal = U.slotOuterWidth /\ U.slotOuterHeight
        ( slotOuterWidth /\ slotOuterHeight ) = slotSize direction

        translateTo (x /\ y) =
            HSA.transform [ HSA.Translate x y ]
        name' =
            HS.g
                [ translateTo $ namePos direction ]
                [ HS.text [] [ HH.text name ] ]
        slot rectPos pos textPos (name /\ shape) =
            HS.g
                []
                [ HS.g
                    [ translateTo pos ]
                    [ HS.circle
                        [ HSA.fill $ Just Colors.slotFill
                        , HSA.stroke $ Just Colors.slotStroke
                        , HSA.strokeWidth $ U.slotStrokeWidth
                        , HSA.r U.slotRadius
                        ]
                    ]
                , HS.g
                    [ translateTo textPos ]
                    [ HS.text [ ] [ HH.text name ] ]
                , HS.g
                    [ translateTo rectPos ]
                    [ HS.rect
                        [ {- HE.onClick
                        , -} HSA.fill $ Just Colors.transparent
                        , HSA.width slotOuterWidth, HSA.height slotOuterHeight
                        ]
                    ]
                ]
        inlets' =
            HS.g [ HSA.classes CS.nodeInlets ]
                $ Array.mapWithIndex inlet' inlets
        inlet' idx (name /\ shape) =
            slot
                (inletRectPos direction idx)
                (inletPos direction idx)
                (inletTextPos direction idx)
                (name /\ shape)
        outlets' =
            HS.g [ HSA.classes CS.nodeOutlets ]
                $ Array.mapWithIndex outlet' outlets
        outlet' idx (name /\ shape) =
            slot
                (outletRectPos direction idx)
                (outletPos direction idx)
                (outletTextPos direction idx)
                (name /\ shape)
        body =
            HS.g
                [ translateTo $ bodyPos direction ]
                [ HS.rect
                    [ HSA.fill $ Just Colors.bodyFill
                    , HSA.stroke $ Just Colors.bodyStroke
                    , HSA.strokeWidth $ U.bodyStrokeWidth
                    , HSA.rx U.bodyCornerRadius, HSA.ry U.bodyCornerRadius
                    , HSA.width innerWidth, HSA.height innerHeight
                    ]
                ]
        shadow =
            HS.g
                [ translateTo $ shadowPos direction ]
                [ HS.rect
                    [ HSA.fill $ Just Colors.bodyShadow
                    , HSA.stroke $ Just Colors.bodyShadow
                    , HSA.strokeWidth $ U.bodyStrokeWidth
                    , HSA.rx U.bodyCornerRadius, HSA.ry U.bodyCornerRadius
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


findBounds :: forall d. Noodle.Node d -> Number /\ Number
findBounds node =
    let
        inletsCount /\ outletsCount = Node.dimensions node
    in
        (U.slotOuterWidth * 2.0 + U.nodeBodyWidth)
        /\ toNumber (max inletsCount outletsCount) * U.slotOuterHeight