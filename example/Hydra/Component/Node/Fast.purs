module Hydra.Component.Node.Fast where


import Prelude

import Effect.Class (class MonadEffect)

-- import Data.String.Read (read)
--import Data.Parse
import Data.Maybe (maybe)
import Data.Tuple.Nested ((/\), type (/\))

import App.Toolkit.UI (FromNode(..)) as UI

import Hydra (Hydra)
import Hydra as Hydra
import Hydra.Component.Input as Input
import Hydra.Toolkit.UI.Components (NodeComponent, NodeInput, NodeOutput) as UI

import App.Emitters as E

import Noodle.Node (Node)

import Halogen as H
import Halogen.Svg.Elements as HS


type State = Number /\ Hydra.Value /\ Node Hydra


data Action
    = NoOp
    | Initialize
    | ChangeSpeed Number
    | ChangeValue Hydra.Value


initialState :: UI.NodeInput -> State
initialState { node } =
    1.0 /\ Hydra.Seq [] /\ node


render :: forall m. MonadEffect m => State -> H.ComponentHTML Action () m
render (num /\ _ /\ _) =
    HS.g
        [ ]
        [ Input.number num { min : 0.0, max : 255.0, step : 0.01 } NoOp ChangeSpeed
        -- , HS.text [] [ HH.text $ show num ]
        ]


handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () UI.NodeOutput m Unit
handleAction = case _ of
    Initialize -> do
        _ /\ _ /\ node <- H.get
        emitter <- E.fromInlet node "value"
        _ <- H.subscribe (maybe NoOp ChangeValue <$> Hydra.toValue <$> emitter)
        pure unit
    ChangeSpeed n -> do
        H.modify_ \(_ /\ v /\ node) -> n /\ v /\ node
        H.raise $ UI.SendToInlet "speed" $ Hydra.num n
    ChangeValue v -> do
        H.modify_ \(n /\ _ /\ node) -> n /\ v /\ node
    NoOp ->
        pure unit


component :: forall m. MonadEffect m => UI.NodeComponent m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }