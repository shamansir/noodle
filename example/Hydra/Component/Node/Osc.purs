module Hydra.Component.Node.Osc where


import Prelude

-- import Data.String.Read (read)
--import Data.Parse
import Effect.Class (class MonadEffect)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec2 (Pos, (<+>))
import Data.Vec2 as V2

import App.Emitters as E

import Noodle.Node (Node)

import Hydra (Hydra)
import Hydra (hydraOf, textureOf, defaultSource) as Hydra
import Hydra.Compile (compile) as Hydra
import Hydra.Component.SineWave as SineWave
import Hydra.Toolkit.UI.Components (NodeComponent, NodeInput, NodeOutput) as UI

import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HSA


type State = Hydra /\ Node Hydra


data Action
    = Initialize
    | Update Hydra


initialState :: UI.NodeInput -> State
initialState { node } =
    (Hydra.hydraOf $ Hydra.textureOf $ Hydra.defaultSource) /\ node


render :: forall m. MonadEffect m => State -> H.ComponentHTML Action () m
render (hydra /\ node) =
    HS.g
        []
        [ ] -- HS.path [ HSA.d $ SineWave.render (0.0 <+> 0.0) 150.0 150.0 3 ] ]


handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () UI.NodeOutput m Unit
handleAction = case _ of
    Initialize -> do
        _ /\ node <- H.get
        emitter <- E.fromOutlet node "osc"
        _ <- H.subscribe (Update <$> emitter)
        pure unit
    Update newHydra ->
        H.modify_ (\(_ /\ node) -> newHydra /\ node)


component :: forall m. MonadEffect m => UI.NodeComponent m
component =
    H.mkComponent
        { initialState
        , render
        , eval:
            H.mkEval H.defaultEval
                { handleAction = handleAction
                , initialize = Just Initialize
                }
        }