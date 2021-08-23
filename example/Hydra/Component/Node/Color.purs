module Hydra.Component.Node.Color where


import Prelude

import Effect.Class (class MonadEffect)

-- import Data.String.Read (read)
--import Data.Parse
import Data.Maybe (fromMaybe)
import Data.Array ((:))
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import App.Toolkit.UI as UI
import App.Emitters as E

import Noodle.Node (Node)
import Noodle.Node as Node

import Hydra (Hydra, Value(..))
import Hydra as Hydra
import Hydra.Extract as HydraE
import Hydra.Component.Input as Input

import Halogen as H
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.None as HS
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes (Color(..)) as S


type State = Array S.Color /\ Node Hydra


data Action
    = Initialize
    | Update Hydra


bodyWidth = 110.0 -- FIXME: pass from outside


initialState :: forall d. UI.NodeInput Hydra -> State
initialState { node } =
    {- Node.defaultOfInlet "seq" node
        <#> HydraE.seq -
         #  fromMaybe [] -}
    [ S.RGB 255 255 255, S.RGB 255 0 0, S.RGB 0 255 0, S.RGB 0 0 255 ] /\ node


render :: forall m. State -> H.ComponentHTML Action () m
render (colors /\ _) =
    HS.g
        []
        [ (HS.g [] $ Array.mapWithIndex colorRect colors)
        ]
    where
        colorsCount = Array.length colors
        colorRectWidth = bodyWidth / toNumber colorsCount
        colorRect i color =
            HS.g
                [ ]
                [ HS.rect
                    [ HSA.x $ colorRectWidth * toNumber i
                    , HSA.y 0.0
                    , HSA.width colorRectWidth
                    , HSA.height 55.0 -- FIXME
                    , HSA.fill $ Just color
                    ]
                ]


handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () (UI.NodeOutput Hydra) m Unit
handleAction = case _ of
    Initialize -> do
        _ /\ node <- H.get
        emitter <- E.fromOutlet node "color"
        _ <- H.subscribe (Update <$> emitter)
        pure unit
    Update hydra -> do
        H.modify_ (\(_ /\ node) -> HydraE.colorMod hydra /\ node)


component :: forall m. MonadEffect m => UI.NodeComponent m Hydra
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
        }