module Web.Components.Library where

import Prelude

import Type.Proxy (Proxy(..))

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
import Web.Formatting (renderFormatting) as WF

import Noodle.Id (FamilyR, family) as Id
import Noodle.Toolkit (class MarkToolkit)

import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette
import Noodle.Ui.Tagging as T

import Web.Components.AppScreen.KeyboardLogic as KL

type Input =
    { families :: Array Id.FamilyR
    , focus :: KL.LibraryFocus
    }


type State =
    { families :: Array Id.FamilyR
    , focus :: KL.LibraryFocus
    }


data Action
    = Initialize
    | RaiseSelectFamily Id.FamilyR
    | Receive Input


data Output
    = SelectFamily Id.FamilyR


component :: forall tk query m. MarkToolkit tk => Proxy tk -> TargetLayer -> H.Component query Input Output m
component ptk layer =
    H.mkComponent
        { initialState
        , render : render ptk layer
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< Receive
            }
        }


initialState :: Input -> State
initialState { families, focus } = { families, focus }


slopeFactor = 5.0 :: Number
fontSize = 12.0 :: Number
width = 125.0 :: Number
height = 900.0 :: Number
headerHeight = 20.0 :: Number
bottomBarHeight = 20.0 :: Number
bodyHeight = height - bottomBarHeight - headerHeight
bodyRelBottomY = bodyHeight :: Number
bottomBarY = height - bottomBarHeight :: Number
paddingLeft = 5.0 :: Number


render :: forall tk m. MarkToolkit tk => Proxy tk -> TargetLayer -> State -> H.ComponentHTML Action () m
render _ SVG state =
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
                [ HSA.transform [ HSA.Translate paddingLeft 0.0 ] ]
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


render ptk HTML state =
    HH.div
        [ HHP.style $ do
            CSS.position CSS.relative
            CSS.left $ CSS.px $ paddingLeft + 5.0
            CSS.top $ CSS.px $ headerHeight + slopeFactor * 2.0
            CSS.width $ CSS.px width
            CSS.maxWidth $ CSS.px width
            CSS.maxHeight $ CSS.px $ bodyHeight - slopeFactor * 4.0
            CSS.overflowY CSS.scroll
            CSS.overflowX CSS.hidden
            CSS.lineHeight $ CSS.px 20.0
            CSS.fontSize $ CSS.px fontSize
            -- CSS.backgroundColor $ C.transparent
            -- HHP.position HHP.Rel { x : 5.0, y : headerHeight + slopeFactor * 2.0 }
        , HHP.class_ $ H.ClassName "library-scrollbar"
        , HHP.tabIndex $ -1
        ]
        $ mapWithIndex familyButton state.families

    where
        familyButton idx familyR =
            let
                familyNameRendered = WF.renderFormatting HTML $ T.libraryItem ptk familyR
                indexMarker color =
                    HH.span
                        [ HHP.style $ do
                            CSS.color $ P.colorOf color
                            -- CSS.borderColor $ P.colorOf $ _.i700 Palette.base_
                            -- CSS.backgroundColor $ P.colorOf $ _.i100 Palette.base_
                            -- CSS.position CSS.absolute
                            CSS.position CSS.absolute
                            CSS.top $ CSS.px (-2.0) -- (-2.0)
                            -- CSS.left $ CSS.px 0.0 -- (3.0)
                            CSS.fontSize $ CSS.em 0.8
                            CSS.margin (CSS.px 0.0) (CSS.px 0.0) (CSS.px 0.0) (CSS.px 4.0)
                            -- CSS.border CSS.solid (CSS.px 1.0) $ P.colorOf $ _.i700 Palette.base_
                            -- CSS.padding (CSS.px 2.0) (CSS.px 2.0) (CSS.px 2.0) (CSS.px 2.0)
                            -- CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
                        ]
                        -- [ HH.text $ show idx ]
                        [ HH.text $ KL.indexToChar idx ]
                selectedIndexMarker =
                    indexMarker $ _.i300 Palette.green
                suggestedIndexMarker =
                    indexMarker $ _.i300 Palette.blue
            in
            HH.span
                [ HHP.style $ do
                    CSS.display CSS.block
                    CSS.position CSS.relative
                    -- CSS.overflow CSS.hidden
                    -- CSS.color $ P.colorOf $ T.ma -- _.i100 Palette.blue
                    CSS.backgroundColor $ P.colorOf $ _.i900 Palette.blue
                , HE.onClick $ const $ RaiseSelectFamily familyR
                ]
                [ case state.focus of
                      KL.FamilySelected n ->
                          if n == idx then
                              HH.span_
                                  [ familyNameRendered
                                  , selectedIndexMarker
                                  ]
                          else
                              familyNameRendered
                              {-
                              HH.span_
                                  [ suggestedIndexMarker
                                  , familyNameRendered
                                  ]
                              -}
                      KL.LibraryOpen ->
                          HH.span_
                              [ familyNameRendered
                              , suggestedIndexMarker
                              ]
                      KL.NoFocusedFamily ->
                          familyNameRendered
                -- HH.text $ Id.family familyR
                ]


handleAction :: forall m. Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
    Initialize -> pure unit
    RaiseSelectFamily familyR -> do
        H.raise $ SelectFamily familyR
    Receive input ->
        H.modify_ _
            { families = input.families
            , focus = input.focus
            }
