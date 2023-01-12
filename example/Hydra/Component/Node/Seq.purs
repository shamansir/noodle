module Hydra.Component.Node.Seq where


import Prelude

import Effect.Class (class MonadEffect)

-- import Data.String.Read (read)
--import Data.Parse
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array ((:))
import Data.Array as Array
import Data.Tuple.Nested (type (/\), (/\))

import Web.App.Toolkit.UI (FromNode(..)) as UI
import Web.Emitters  as E

import Noodle.Node as Node
import Noodle.Node (Node)

import Hydra (Hydra, Value(..))
import Hydra as Hydra
import Hydra.Extract as HydraE
import Hydra.Component.Input as Input
import Hydra.Toolkit.UI.Components (NodeComponent, NodeInput, NodeOutput) as UI

import Halogen as H
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.None as HS


type State = Array Value /\ Node Hydra


data Action
    = NoOp
    | Initialize
    -- | Add
    | Change Int Number
    | Remove Int
    | Update Hydra


initialState :: UI.NodeInput -> State
initialState { node } =
    (Node.defaultOfInlet "seq" node
        <#> HydraE.seq
         #  fromMaybe []
    ) /\ node


render :: forall m. State -> H.ComponentHTML Action () m
render (numbers /\ _) =
    HS.g
        []
        [ (HS.g [] $ Array.mapWithIndex itemInput numbers)
        -- , Input.button Add
        ]
    where
        itemInput i (Num n) =
            HS.g
                [ ]
                [ Input.number n { min : 0.0, max : 255.0, step : 0.01 } NoOp $ Change i
                , Input.button $ Remove i
                ]
        itemInput _ _ =
            HS.none


handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () UI.NodeOutput m Unit
handleAction = case _ of
    NoOp ->
        pure unit
    Initialize -> do
        _ /\ node <- H.get
        emitter <- E.fromOutlet node "seq"
        _ <- H.subscribe (Update <$> emitter)
        pure unit
    {- Add -> do
        H.modify_ ((:) (Num 0.0))
        next <- H.get
        H.raise $ UI.SendToOutlet "seq" $ Hydra.seq next -}
    Change i n -> do
        H.modify_ $ \(a /\ node) -> (Array.updateAt i (Num n) a # fromMaybe a) /\ node
        next /\ _ <- H.get
        H.raise $ UI.SendToOutlet "seq" $ Hydra.seq next
    Update hydra -> do
        H.modify_ (\(_ /\ node) -> HydraE.seq hydra /\ node)
    Remove i -> do
        H.modify_ $ \(a /\ node) -> (Array.deleteAt i a # fromMaybe a) /\ node
        next /\ _ <- H.get
        H.raise $ UI.SendToOutlet "seq" $ Hydra.seq next


component :: forall m. MonadEffect m => UI.NodeComponent m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
        }