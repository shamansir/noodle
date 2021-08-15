module App.Component.Link where


import Prelude (identity)

import Data.Unit (Unit, unit)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))

import Noodle.Network as Noodle

import Noodle.Node as Node
import Noodle.Node.Shape (InletId, OutletId)

import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS


type Input =
    { outlet :: Maybe (Node.Id /\ InletId)
    , inlet :: Maybe (Node.Id /\ OutletId)
    }


type State =
    { outlet :: Maybe (Node.Id /\ InletId)
    , inlet :: Maybe (Node.Id /\ OutletId)
    -- TODO: positions, etc
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


{-
function bezierByH(x0, y0, x1, y1) {
    var mx = x0 + (x1 - x0) / 2;

    return 'M' + x0 + ' ' + y0 + ' '
         + 'C' + mx + ' ' + y0 + ' '
               + mx + ' ' + y1 + ' '
               + x1 + ' ' + y1;
}
-}