module Hydra.Component.Node.Palette where


import Prelude

import Effect.Class (class MonadEffect)
import Color as C
import Math (floor)

-- import Data.String.Read (read)
--import Data.Parse
import Data.Maybe (fromMaybe)
import Data.Array ((:))
import Data.Array as Array
import Data.Map as Map
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec ((!!))
import Data.Typelevel.Num.Reps (d0, d1, d2)

import App.Toolkit.UI as UI
import App.Emitters as E

import JetBrains.Palettes as P

import Noodle.Node (Node)
import Noodle.Node as Node

import Hydra (Hydra, Value(..))
import Hydra as Hydra
import Hydra.Extract as HydraE
import Hydra.Component.Input as Input

import Halogen as H
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Svg.Elements as HS
import Halogen.Svg.Elements.None as HS
import Halogen.Svg.Attributes as HSA

import Color.Extra (toSvg) as C


type State = Mode /\ P.PaletteId


data Mode
    = Modifier
    | Solid


data Action
    = Change P.PaletteId


bodyWidth = 110.0 -- FIXME: pass from outside


-- defaultPalette :: Hydra.Color
-- defaultPalette = Hydra.Color { r = Num


initialState :: forall patch_state. Mode -> UI.NodeInput' patch_state Hydra -> State
initialState mode _ =
    mode /\ "JetBrains"


render :: forall m. State -> H.ComponentHTML Action () m
render (_ /\ paletteId) =
    HS.g
        [ HSA.class_ $ H.ClassName "palette-node" ]
        [ (HS.g [] $ Array.mapWithIndex colorRect colors)
        , (HS.g [] $ Array.mapWithIndex paletteButton $ Map.toUnfoldable P.palettes)
        ]
    where
        paletteButton idx (paletteId /\ _) =
            HS.g
                [ HSA.class_ $ H.ClassName "palette-button" ]
                [ HS.circle
                    [ HSA.cx $ 7.0 + (toNumber $ idx `mod` 9) * 12.0
                    , HSA.cy $ 7.0 + (floor $ toNumber idx / 9.0) * 12.0
                    , HSA.stroke $ Just $ C.toSvg $ C.rgba 0 0 0 0.5
                    , HSA.fill $ Just $ C.toSvg $ C.rgba 0 0 0 0.0
                    , HSA.strokeWidth 1.0
                    , HSA.r 5.0
                    ]
                , HS.rect
                    [ HSA.x $ 2.0 + (toNumber $ idx `mod` 9) * 12.0
                    , HSA.y $ 2.0 + (floor $ toNumber idx / 9.0) * 12.0
                    , HSA.width 10.0
                    , HSA.height 10.0
                    , HSA.stroke $ Just $ C.toSvg $ C.rgba 0 0 0 0.0
                    , HSA.fill $ Just $ C.toSvg $ C.rgba 0 0 0 0.0
                    , HE.onClick $ const $ Change paletteId
                    -- , HP.style "cursor: pointer"
                    ]
                ]
        colors = P.toArray palette
        palette = fromMaybe P.default $ Map.lookup paletteId P.palettes
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
                    , HSA.fill $ Just $ C.toSvg color
                    ]
                ]


handleAction :: forall patch_action m. MonadEffect m => Action -> H.HalogenM State Action () (UI.NodeOutput' patch_action Hydra) m Unit
handleAction = case _ of
    Change paletteId -> do
        mode /\ _ <- H.get
        H.modify_ (\(mode /\ _) -> mode /\ paletteId)
        let palette = fromMaybe P.default $ Map.lookup paletteId P.palettes
        H.raise $ UI.SendToInlet "palette" $ case mode of
            Modifier -> Hydra.justModifier $ Hydra.color $ P.toColorMod palette
            Solid -> Hydra.hydraOf $ Hydra.textureOf $ P.toSolidSource palette


component :: forall patch_action patch_state m. MonadEffect m => Mode -> UI.NodeComponent' patch_action patch_state Hydra m
component mode =
    H.mkComponent
        { initialState : initialState mode
        , render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }