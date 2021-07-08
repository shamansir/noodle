module App.Component.Node where


import Prelude

import Data.Unit (Unit, unit)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Data.Array as Array
import Data.Int (toNumber)

import Noodle.Node (Node) as Noodle
import Noodle.Node as Node

import App.Colors as Colors

import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HSA

import Type.Proxy (Proxy(..))


type Slot id = forall query. H.Slot query Void id


type Input d =
    { node :: Noodle.Node d
    }


type State d =
    { node :: Noodle.Node d
    }


data Action d
    = Receive (Input d)


initialState :: forall d. Input d -> State d
initialState = identity


render :: forall d m. State d -> H.ComponentHTML (Action d) () m
render { node } =
    HS.g
        []
        [ inlets'
        , body
        , outlets'
        ]
    where
        inlets = Node.inlets node
        outlets = Node.outlets node
        inletsCount = toNumber $ Array.length inlets
        outletsCount = toNumber $ Array.length outlets
        slotWidth = 20.0
        slotHeight = 20.0
        width = 50.0
        height = max inletsCount outletsCount * slotHeight
        inlets' =
            HS.g [ HSA.class_ $ H.ClassName "inlets" ]
                $ Array.mapWithIndex inlet' inlets
        inlet' idx (name /\ shape) =
            HS.g
                [ HSA.transform [ HSA.Translate 0.0 $ slotHeight * toNumber idx ] ]
                [ HS.text [] [ HH.text name ] ]
        outlets' =
            HS.g [ HSA.class_ $ H.ClassName "outlets" ]
                $ Array.mapWithIndex outlet' outlets
        outlet' idx (name /\ shape) =
            HS.g
                [ HSA.transform [ HSA.Translate 0.0 $ slotHeight * toNumber idx ] ]
                [ HS.text [] [ HH.text name ] ]
        body = HS.g [] []


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