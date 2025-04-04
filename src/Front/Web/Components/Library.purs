module Web.Components.Library where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Array ((:))
import Data.Array (snoc) as Array
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

import Web.Paths as Paths

import Noodle.Id (FamilyR, family) as Id

import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette


type Input =
    { families :: Array Id.FamilyR
    }


type State =
    { families :: Array Id.FamilyR
    }


data Action
    = Initialize
    | RaiseSelectFamily Id.FamilyR
    | Receive Input


data Output
    = SelectFamily Id.FamilyR


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
initialState { families } = { families }


width = 110.0 :: Number
height = 900.0 :: Number


render :: forall m. State -> H.ComponentHTML Action () m
render state =
    HS.g
        [ HSA.transform [ HSA.Translate 5.0 patchesBarHeight ] ]
        [ backdrop, familyButtonsGroup ]
    where

        slopeFactor = 5.0
        patchesBarHeight = 45.0
        headerHeight = 20.0
        bottomBarHeight = 20.0
        bodyRelBottomY = height - bottomBarHeight - headerHeight
        bottomBarY = height - bottomBarHeight

        familyButtonsGroup =
            HS.g
                [ HSA.transform [ HSA.Translate 5.0 $ headerHeight + slopeFactor * 2.0 ] ]
                $ mapWithIndex familyButton state.families

        backdrop =
            HS.g
                [ HSA.transform [ HSA.Translate 0.0 0.0 ] ]
                [ HS.path
                    [ HSA.d $ Paths.libraryTop { slope : slopeFactor, width, height : headerHeight }
                    , HSA.fill $ Just $ P.hColorOf $ _.i900 $ Palette.magenta
                    , HSA.stroke $ Just $ P.hColorOf $ _.i200 Palette.magenta
                    , HSA.strokeWidth 1.0
                    ]
                , HS.text
                    [ HSA.stroke $ Just $ P.hColorOf $ _.i50 Palette.blue
                    , HSA.x 6.0
                    , HSA.y 7.0
                    , HSA.dominant_baseline HSA.Hanging
                    ]
                    [ HH.text "LIBRARY" ]
                , HS.g
                    [ HSA.transform [ HSA.Translate 0.0 bottomBarY ] ]
                    [ HS.path
                        [ HSA.d $ Paths.libraryBottom { slope : slopeFactor, width, height : bottomBarHeight }
                        , HSA.fill $ Just $ P.hColorOf $ _.i900 $ Palette.magenta
                        , HSA.stroke $ Just $ P.hColorOf $ _.i200 Palette.magenta
                        , HSA.strokeWidth 1.0
                        ]
                    ]
                , HS.g
                    [ HSA.transform [ HSA.Translate 0.0 headerHeight ] ]
                    [ HS.path
                        [ HSA.d $ Paths.libraryBody { slope : slopeFactor, width, height : bodyRelBottomY }
                        , HSA.fill $ Just $ P.hColorOf $ _.i900 $ Palette.blue
                        , HSA.stroke $ Just $ P.hColorOf $ _.i200 Palette.blue
                        , HSA.strokeWidth 1.0
                        ]
                    ]
                ]

        familyButton idx familyR =
            HS.g
                [
                ]
                [ HS.text
                    [ HSA.fill $ Just $ P.hColorOf $ _.i100 Palette.blue
                    , HSA.y $ Int.toNumber idx * 20.0
                    , HSA.dominant_baseline HSA.Hanging
                    , HE.onClick $ const $ RaiseSelectFamily familyR
                    ]
                    [ HH.text $ Id.family familyR
                    ]
                ]


handleAction :: forall m. Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
    Initialize -> pure unit
    RaiseSelectFamily familyR -> do
        H.raise $ SelectFamily familyR
    Receive input ->
        H.modify_ _
            { families = input.families
            }
