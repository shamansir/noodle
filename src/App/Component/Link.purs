module App.Component.Link where


import Prelude

import Data.Unit (Unit, unit)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Data.Vec2 (Pos)

import Noodle.Network as Noodle

import Noodle.Node as Node
import Noodle.Node.Shape (InletId, OutletId)

import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HSA


import App.Style (LinkStyle)


type Slot id = forall query. H.Slot query Void id


type Input =
    { outlet :: Node.Id /\ OutletId
    , inlet :: Maybe (Node.Id /\ InletId)
    , style :: LinkStyle
    , startPos :: Pos
    , endPos :: Pos
    }


type State =
    { outlet :: Node.Id /\ OutletId
    , inlet :: Maybe (Node.Id /\ InletId)
    , style :: LinkStyle
    , startPos :: Pos
    , endPos :: Pos
    }


data Action
    = Disconnect
    | Drag (Int /\ Int)


initialState :: Input -> State
initialState = identity


render :: forall m. State -> H.ComponentHTML Action () m
render _ = HS.svg [] []


handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Disconnect ->
    H.modify_ \state -> state
  Drag pos ->
    H.modify_ \state -> state


component :: forall query output m. H.Component query Input output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }


bezierBy :: { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number } -> Array HSA.PathCommand
bezierBy { x0, y0, x1, y1 } =
    [ HSA.m HSA.Abs x0 y0
    , HSA.c HSA.Abs mx y0 mx y1 x1 y1
    ]
    where mx = x0 + (x1 - x0) / 2.0