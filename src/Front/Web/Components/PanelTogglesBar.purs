module Web.Components.PanelTogglesBar where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Array (snoc, elem, length) as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (length) as String
import Data.Int (toNumber) as Int
import Data.Foldable (foldl)
import Data.FunctorWithIndex (mapWithIndex)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS

import Front.Shared.Panels (Which(..), allPanels) as Panels

import Noodle.Id (PatchR, PatchName) as Id

import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette


type Input =
    { openPanels :: Array Panels.Which
    }


type State =
    { openPanels :: Array Panels.Which
    }


data Action
    = Initialize
    | RaiseOpenPanel Panels.Which
    | RaiseClosePanel Panels.Which
    | Receive Input


data Output
    = OpenPanel Panels.Which
    | ClosePanel Panels.Which


component :: forall query m. H.Component query Input Output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< Receive
            }
        }


initialState :: Input -> State
initialState { openPanels } = { openPanels }


height = 25.0 :: Number


render :: forall m. State -> H.ComponentHTML Action () m
render state =
    HS.g
        [ ]
        $ mapWithIndex panelButton Panels.allPanels
    where
        barPadding = 7.0
        buttonPadding = 5.0
        symWidth = 10.5
        slopeFactor = 5.0
        buttonHeight = height
        fontSize = 12.0
        textY = (buttonHeight / 2.0) + 1.0

        buttonWidth = 30.0
        panelsCount = Array.length Panels.allPanels
        togglesBarWidth = Int.toNumber panelsCount * (buttonWidth + buttonPadding)

        panelButton index which =
            HS.g
                [ HSA.transform [ HSA.Translate ((Int.toNumber index / Int.toNumber panelsCount) * togglesBarWidth) barPadding ]
                , HE.onClick $ const $
                    if Array.elem which state.openPanels
                        then RaiseOpenPanel which
                        else RaiseClosePanel which
                ]
                [ HS.rect
                    [ HSA.x 0.0, HSA.y 0.0
                    , HSA.rx slopeFactor, HSA.ry slopeFactor
                    , HSA.width 30.0, HSA.height buttonHeight
                    , HSA.fill $ Just $ P.hColorOf $ _.i800 Palette.base_
                    , HSA.stroke $ Just $ P.hColorOf $ _.i100 Palette.blue
                    , HSA.strokeWidth 2.0
                    ]
                , HS.text
                    [ HSA.x 10.0, HSA.y textY
                    , HSA.font_size $ HSA.FontSizeLength $ HSA.Px 14.0
                    , HSA.fill $ Just $ P.hColorOf $ _.i100 Palette.blue
                    , HSA.dominant_baseline HSA.BaselineMiddle
                    ]
                    [ HH.text "+" ]
                ]


handleAction :: forall m. Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
    Initialize -> pure unit
    RaiseOpenPanel which -> do
        H.raise $ OpenPanel which
    RaiseClosePanel which -> do
        H.raise $ ClosePanel which
    Receive input ->
        H.modify_ _
            { openPanels = input.openPanels
            }
