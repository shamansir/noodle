module Hydra.Component.Node.Osc where


import Prelude

-- import Data.String.Read (read)
--import Data.Parse
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested ((/\), type (/\))

import App.UI as UI

import Noodle.Node (Node)

import Hydra (Hydra)
import Hydra (hydraOf, entityOf, defaultSource) as Hydra
import Hydra.Compile (compile) as Hydra

import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS


type State = Hydra /\ Node Hydra


data Action
    = Unit


initialState :: UI.NodeInput Hydra -> State
initialState node =
    (Hydra.hydraOf $ Hydra.entityOf $ Hydra.defaultSource) /\ node


render :: forall m. State -> H.ComponentHTML Action () m
render (hydra /\ node) =
    HS.g
        []
        [ HS.text [] [ HH.text $ fromMaybe "-" $ Hydra.compile hydra ] ]


handleAction :: forall m. Action -> H.HalogenM State Action () (UI.NodeOutput Hydra) m Unit
handleAction = const $ pure unit


component :: forall m. UI.NodeComponent m Hydra
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }