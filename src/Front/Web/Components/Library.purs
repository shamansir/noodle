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

-- import Color (transparent) as C
import CSS as CSS
import CSS.Overflow as CSS
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties hiding (style) as HHP
import Halogen.HTML.CSS as HHP
import Halogen.HTML.Properties.Extra as HHP
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS

import Web.Paths as Paths
import Web.Layer (TargetLayer(..))

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


component :: forall query m. TargetLayer -> H.Component query Input Output m
component layer =
    H.mkComponent
        { initialState
        , render : render layer
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< Receive
            }
        }


initialState :: Input -> State
initialState { families } = { families }


slopeFactor = 5.0 :: Number
fontSize = 12.0 :: Number
width = 125.0 :: Number
height = 900.0 :: Number
headerHeight = 20.0 :: Number
bottomBarHeight = 20.0 :: Number
bodyHeight = height - bottomBarHeight - headerHeight
bodyRelBottomY = bodyHeight :: Number
bottomBarY = height - bottomBarHeight :: Number


render :: forall m. TargetLayer -> State -> H.ComponentHTML Action () m
render SVG state =
    HS.g
        [ ]
        -- [ backdrop, familyButtonsGroup ]
        [ backdrop ]
    where
        {-
        familyButtonsGroup =
            HS.g
                [ HSA.transform [ HSA.Translate 5.0 $ headerHeight + slopeFactor * 2.0 ] ]
                $ mapWithIndex familyButton state.families
        -}

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
                    [ HSA.fill $ Just $ P.hColorOf $ _.i50 Palette.blue
                    , HSA.x 6.0
                    , HSA.y 7.0
                    , HSA.font_size $ HSA.FontSizeLength $ HSA.Px fontSize
                    , HSA.dominant_baseline HSA.Hanging
                    ]
                    [ HH.text ": LIBRARY" ]
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

        {-
        familyButton idx familyR =
            HS.g
                [
                ]
                [ HS.text
                    [ HSA.fill $ Just $ P.hColorOf $ _.i100 Palette.blue
                    , HSA.y $ Int.toNumber idx * 20.0
                    , HSA.dominant_baseline HSA.Hanging
                    , HSA.font_size $ HSA.FontSizeLength $ HSA.Px fontSize
                    , HE.onClick $ const $ RaiseSelectFamily familyR
                    ]
                    [ HH.text $ Id.family familyR
                    ]
                ]
        -}


render HTML state =
    HH.div
        [ HHP.style $ do
            CSS.position CSS.relative
            CSS.left $ CSS.px 5.0
            CSS.top $ CSS.px $ headerHeight + slopeFactor * 2.0
            CSS.maxWidth $ CSS.px width
            CSS.maxHeight $ CSS.px $ bodyHeight - slopeFactor * 4.0
            CSS.overflowY CSS.scroll
            CSS.overflowX CSS.hidden
            CSS.lineHeight $ CSS.px 20.0
            CSS.fontSize $ CSS.px fontSize
            -- CSS.backgroundColor $ C.transparent
            -- HHP.position HHP.Rel { x : 5.0, y : headerHeight + slopeFactor * 2.0 }
        , HHP.class_ $ H.ClassName "library-scrollbar"
        ]
        $ mapWithIndex familyButton state.families

    where
        familyButton idx familyR =
            HH.span
                [ HHP.style $ do
                    CSS.display CSS.block

                    -- CSS.overflow CSS.hidden
                    CSS.color $ P.colorOf $ _.i100 Palette.blue
                -- HSA.fill $ Just $ P.hColorOf $ _.i100 Palette.blue
                -- , HSA.y $ Int.toNumber idx * 20.0
                -- , HSA.dominant_baseline HSA.Hanging
                -- , HSA.font_size $ HSA.FontSizeLength $ HSA.Px fontSize
                , HE.onClick $ const $ RaiseSelectFamily familyR
                ]
                [ HH.text $ Id.family familyR
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
