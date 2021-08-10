module Hydra.Component.Node.Num where


import Prelude

import Effect.Class (class MonadEffect)

-- import Data.String.Read (read)
--import Data.Parse
import Data.Maybe (fromMaybe)

import App.Toolkit.UI as UI

import Hydra (Hydra)
import Hydra as Hydra
import Hydra.Extract as HydraE
import Hydra.Component.Input as Input

import Noodle.Node as Node

import Halogen as H
import Halogen.Svg.Elements as HS


type State = Number


data Action
    = NoOp
    | Change Number


initialState :: UI.NodeInput Hydra -> State
initialState { node } =
    Node.defaultOfInlet "num" node
        <#> HydraE.numOr 0.0
         #  fromMaybe 0.0


render :: forall m. MonadEffect m => State -> H.ComponentHTML Action () m
render num =
    HS.g
        [ ]
        [ Input.number num { min : 0.0, max : 255.0, step : 0.01 } NoOp Change
        -- , HS.text [] [ HH.text $ show num ]
        ]


handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () (UI.NodeOutput Hydra) m Unit
handleAction = case _ of
    Change n -> do
        H.put n
        H.raise $ UI.SendToOutlet "num" $ Hydra.num n
    NoOp ->
        pure unit


component :: forall m. MonadEffect m => UI.NodeComponent m Hydra
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }