module Web.Components.PatchesBar where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Array (snoc) as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (length) as String
import Data.Int (toNumber) as Int
import Data.Foldable (foldl)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS

import Noodle.Id (PatchR, PatchName) as Id

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
    | RaiseSelectPatch Id.PatchR
    | RaiseCreatePatch
    | Receive Input


data Output
    = SelectPatch Id.PatchR
    | CreatePatch


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
        slopeFactor = 5.0
        buttonHeight = 25.0
        fontSize = 12.0
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
                , HE.onClick $ const $ RaiseSelectPatch id
                ]
                [ HS.path
                    [ HSA.d
                        [ HSA.m HSA.Abs 0.0 0.0
                        , HSA.l HSA.Abs (width - slopeFactor) 0.0
                        , HSA.l HSA.Abs width slopeFactor
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
                , HE.onClick $ const $ RaiseCreatePatch
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
    RaiseSelectPatch patchR -> do
        H.raise $ SelectPatch patchR
    RaiseCreatePatch -> do
        H.raise CreatePatch
    Receive input ->
        H.modify_ _
            { patches = input.patches
            , selected = input.selected
            }
