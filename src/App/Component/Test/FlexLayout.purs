module App.Component.Test.FlexLayout where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff)
import Type.Row (type (+))

import Color as C
import Color.Extra as C

import App.Layout.Flex (Flex)
import App.Layout.Flex as Flex

import Data.Array as Array

import Data.Int (toNumber, floor)
import Data.Number
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe as Maybe
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec2 (Vec2, Pos, Size, (<+>))
import Data.Vec2 as V2
import Data.Foldable (foldr)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.None as HS


type Slot id = forall query. H.Slot query Void id


type ColoredBlock = Int


type State =
    Array (String /\ Flex Size ColoredBlock)


data Action
    = Initialize


initialState :: State
initialState = []


render
    :: forall m slots
     . MonadEffect m
    => State
    -> H.ComponentHTML Action slots m
render state =
    HS.g
        []
        []

handleAction
    :: forall slots output m
     . Action
    -> H.HalogenM State Action slots output m Unit
handleAction = case _ of

    Initialize -> do
        pure unit


component
    :: forall query input output m
     . MonadEffect m
    => H.Component query input output m
component =
    H.mkComponent
        { initialState : const initialState
        , render
        , eval:
            H.mkEval H.defaultEval
                { handleAction = handleAction
                , initialize = Just Initialize
                }
        }
