module App.Component.Test.FlexLayout where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff)
import Type.Row (type (+))

import Color as C
import Color.Extra as C

import App.Layout.Flex (Flex)
import App.Layout.Flex as Flex
import App.Layout.Flex.Build as Flex

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
import App.Svg.Extra as HSA
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.None as HS


type Slot id = forall query. H.Slot query Void id


type ColoredBlock = C.Color


type ColoredFlex = Flex Size ColoredBlock


type State =
    Array (String /\ ColoredFlex)


data Action
    = Initialize


initialState :: State
initialState = map (Flex.layout $ 300.0 <+> 20.0) <$>
    [ "[ fill black ]" /\
        Flex.vert
            [ Flex.fill /\
                Flex.horz [ Flex.fill /\ C.black ]
            ]
    ]


renderFlex ::forall m slots. ColoredFlex -> H.ComponentHTML Action slots m
renderFlex =
    const $ HS.g [] []
    -- Flex.foldWithPos ?wh (HS.g [] [])



render
    :: forall m slots
     . MonadEffect m
    => State
    -> H.ComponentHTML Action slots m
render state =
    HS.g
        []
        [ HS.text
                [ HSA.translateTo' $ 200.0 <+> 0.0
                , HSA.class_ $ H.ClassName "debug"
                ]
                [ HH.text $ show "foo" ]
        ]

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
