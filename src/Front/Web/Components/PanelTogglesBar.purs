module Web.Components.PanelTogglesBar where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (length) as Array
import Data.Map (Map)
import Data.Map (lookup) as Map
import Data.Set (Set)
import Data.Set (member, size) as Set
import Data.String.CodeUnits as CU
import Data.Int (toNumber) as Int
import Data.FunctorWithIndex (mapWithIndex)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS

import Front.Shared.Panels (Which, allPanels) as Panels

import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette


type Input =
    { openPanels :: Set Panels.Which
    , symbols :: Map Panels.Which Char
    }


type State =
    { openPanels :: Set Panels.Which
    , symbols :: Map Panels.Which Char
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
initialState { openPanels, symbols } = { openPanels, symbols }


buttonWidth = 30.0 :: Number
buttonHeight = height :: Number
buttonPadding = 5.0 :: Number


width = Int.toNumber panelsCount_ * (buttonWidth + buttonPadding) :: Number
height = 25.0 :: Number


panelsCount_ = Array.length Panels.allPanels :: Int


render :: forall m. State -> H.ComponentHTML Action () m
render state =
    HS.g
        [ ]
        $ mapWithIndex panelButton Panels.allPanels
    where
        barPadding = 7.0
        slopeFactor = 5.0
        fontSize = 12.0
        textY = (buttonHeight / 2.0) + 1.0

        panelButton index which =
            HS.g
                [ HSA.transform [ HSA.Translate ((Int.toNumber index / Int.toNumber panelsCount_) * width) barPadding ]
                , HE.onClick $ const $
                    if Set.member which state.openPanels
                        then RaiseClosePanel which
                        else RaiseOpenPanel which
                ]
                [ HS.rect
                    [ HSA.x 0.0, HSA.y 0.0
                    , HSA.rx slopeFactor, HSA.ry slopeFactor
                    , HSA.width buttonWidth, HSA.height buttonHeight
                    , HSA.fill $ Just $ P.hColorOf $ if Set.member which state.openPanels then _.i900 Palette.base_ else _.i800 Palette.base_
                    , HSA.stroke $ Just $ P.hColorOf $ _.i100 Palette.blue
                    , HSA.strokeWidth 2.0
                    ]
                , HS.g
                    [ HSA.transform [ HSA.Translate 10.0 textY ] ]
                    [ ]
                , HS.text
                    [ HSA.x 10.0, HSA.y textY
                    , HSA.font_size $ HSA.FontSizeLength $ HSA.Px 14.0
                    , HSA.fill $ Just $ P.hColorOf $ {- if Set.member which state.openPanels then _.i100 Palette.green else -} _.i100 Palette.blue
                    , HSA.dominant_baseline HSA.BaselineMiddle
                    ]
                    [ HH.text $ panelSymbol which ]
                ]

        panelSymbol =
            CU.singleton <<< fromMaybe '?' <<< flip Map.lookup state.symbols


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
            , symbols = input.symbols
            }
