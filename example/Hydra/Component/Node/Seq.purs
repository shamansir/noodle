module Hydra.Component.Node.Seq where


import Prelude

-- import Data.String.Read (read)
--import Data.Parse
import Data.Maybe (fromMaybe)
import Data.Array ((:))
import Data.Array as Array

import App.Toolkit.UI as UI

import Noodle.Node as Node

import Hydra (Hydra, Value(..))
import Hydra as Hydra
import Hydra.Extract as HydraE
import Hydra.Component.Input as Input

import Halogen as H
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.None as HS


type State = Array Value


data Action
    = NoOp
    | Add
    | Change Int Number
    | Remove Int


initialState :: UI.NodeInput Hydra -> State
initialState { node } =
    {- Node.defaultOfInlet "seq" node
        <#> HydraE.seq -
         #  fromMaybe [] -}
    []


render :: forall m. State -> H.ComponentHTML Action () m
render numbers =
    HS.g
        []
        [ (HS.g [] $ Array.mapWithIndex itemInput numbers)
        , Input.button Add
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


handleAction :: forall m. Action -> H.HalogenM State Action () (UI.NodeOutput Hydra) m Unit
handleAction = case _ of
    NoOp ->
        pure unit
    Add -> do
        H.modify_ ((:) (Num 0.0))
        next <- H.get
        H.raise $ UI.SendToOutlet "seq" $ Hydra.seq next
    Change i n -> do
        H.modify_ $ \a -> Array.updateAt i (Num n) a # fromMaybe a
        next <- H.get
        H.raise $ UI.SendToOutlet "seq" $ Hydra.seq next
    Remove i -> do
        H.modify_ $ \a -> Array.deleteAt i a # fromMaybe a
        next <- H.get
        H.raise $ UI.SendToOutlet "seq" $ Hydra.seq next


component :: forall m. UI.NodeComponent m Hydra
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }