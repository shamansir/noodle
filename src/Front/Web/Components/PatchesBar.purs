module Web.Components.PatchesBar where

import Prelude

import Effect.Class (class MonadEffect)

import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst, snd) as Tuple
import Data.Array (snoc) as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (length) as String
import Data.Int (toNumber) as Int
import Data.FunctorWithIndex (mapWithIndex)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)

import Control.Monad.State (modify, put) as State

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize as HSA
import Halogen.Svg.Attributes.Color as HC
import Halogen.Svg.Elements as HS

import Noodle.Id (PatchR, PatchName) as Id
import Noodle.Patch (Patch)
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (families, class HoldsFamilies) as Toolkit
import Noodle.Network (addPatch) as Network

import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette


type Input =
    { patches :: Array (Id.PatchR /\ Id.PatchName)
    , selected :: Maybe Id.PatchR
    }


type State =
    { patches :: Array (Id.PatchR /\ Id.PatchName)
    , selected :: Maybe Id.PatchR
    }


data Action
    = Initialize
    | SelectPatch Id.PatchR
    | Receive Input


data Output
    = SelectPatchO Id.PatchR


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
initialState { patches, selected } = { patches, selected }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HS.g
    [ ]
    $ Array.snoc
        ( patchButton <$> patchesWithDimensions )
        $ addPatchButton $ buttonsOffset + buttonPadding
    where
        barPadding = 7.0
        buttonPadding = 5.0
        symWidth = 10.5
        buttonCorner = 10.0
        buttonHeight = 30.0
        fontSize = 14.0
        textY = (buttonHeight / 2.0) + 1.0
        isSelected patchR = maybe false (_ == patchR) state.selected
        computeOffset (prevOffset /\ items) (id /\ name) =
            let
                width = (Int.toNumber (String.length name) * symWidth + 2.0)
                nextOffset = prevOffset + width + buttonPadding
            in
                nextOffset /\ Array.snoc items { name, id, offset : prevOffset, width }
        buttonsOffset /\ patchesWithDimensions = foldl computeOffset (barPadding /\ []) state.patches
        patchButton { offset, width, id, name } =
            HS.g
                [ HSA.transform [ HSA.Translate offset barPadding ]
                , HE.onClick $ const $ SelectPatch id
                ]
                [ HS.path
                    [ HSA.d
                        [ HSA.m HSA.Abs 0.0 0.0
                        , HSA.l HSA.Abs (width - buttonCorner) 0.0
                        , HSA.l HSA.Abs width buttonCorner
                        , HSA.l HSA.Abs width buttonHeight
                        , HSA.l HSA.Abs 0.0 buttonHeight
                        , HSA.z
                        ]
                    -- , HSA.x 0.0, HSA.y 0.0
                    -- , HSA.width width, HSA.height buttonHeight
                    , HSA.fill $ Just $ P.hColorOf $ (if isSelected id then _.i600 else _.i800) $ Palette.blue
                    , HSA.stroke $ Just $ P.hColorOf $ _.i100 Palette.blue
                    , HSA.strokeWidth 2.0
                    ]
                , HS.text
                    [ HSA.x 6.0, HSA.y textY
                    , HSA.font_size $ HSA.FontSizeLength $ HSA.Px fontSize
                    , HSA.fill $ Just $ P.hColorOf $ _.i100 Palette.blue
                    , HSA.dominant_baseline HSA.BaselineMiddle
                    ]
                    [ HH.text name ]
                ]
        addPatchButton offset =
            HS.g
                [ HSA.transform [ HSA.Translate offset barPadding ]
                ]
                [ HS.rect
                    [ HSA.x 0.0, HSA.y 0.0
                    , HSA.rx buttonCorner, HSA.ry buttonCorner
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
    SelectPatch patchR -> do
        H.raise $ SelectPatchO patchR
    Receive input ->
        H.modify_ _
            { patches = input.patches
            , selected = input.selected
            }
