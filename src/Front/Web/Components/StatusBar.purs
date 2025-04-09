module Web.Components.StatusBar where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (take) as String

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Properties.Extra (Position(..), position_, fontSize_) as HHP
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS

import Noodle.Id (LinkR) as Id
import Noodle.Raw.Link (Connector) as RawLink
import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette

import Data.Text.Format (Tag) as T

import Web.Paths (statusBar) as P
import Web.Formatting as WF
import Web.Layer (TargetLayer(..))


type Input =
    { content :: T.Tag
    , width :: Number
    , currentZoom :: Number
    }


type State =
    { content :: T.Tag
    , width :: Number
    , currentZoom :: Number
    }


data Action
    = Receive Input
    | RequestToResetZoom


data Output
    = ResetZoom


data Query a
    = UpdateContent T.Tag a


height = 25.0


component :: forall m. TargetLayer -> H.Component Query Input Output m
component layer =
    H.mkComponent
        { initialState
        , render : render layer
        , eval: H.mkEval H.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            , receive = Just <<< Receive
            }
        }


initialState :: Input -> State
initialState { content, width, currentZoom } = { content, width, currentZoom }


render :: forall m. TargetLayer -> State -> H.ComponentHTML Action () m
render SVG state =
    HS.g
        []
        [ HS.path
            [ HSA.d $ P.statusBar { slope : slopeFactor, height, width : state.width }
            , HSA.fill $ Just $ P.hColorOf $ _.i950 Palette.yellow
            , HSA.stroke $ Just $ P.hColorOf $ _.i800 Palette.yellow
            ]
        , if state.currentZoom /= 1.0 then
            HS.g
                [ HSA.transform
                    [ HSA.Translate (state.width - zoomTextWidth - 10.0) 10.0 ] ]
                [ HS.text
                    [ HSA.fill $ Just $ P.hColorOf $ _.i100 Palette.green
                    , HSA.font_size $ HSA.FontSizeLength $ HSA.Px fontSize
                    , HSA.dominant_baseline HSA.Central
                    ]
                    [ HH.text $ String.take 5 $ show state.currentZoom ]
                , HS.circle
                    [ HSA.fill $ Just $ P.hColorOf $ _.i100 Palette.green
                    , HSA.r 5.0
                    , HSA.cx $ zoomTextWidth + 5.0
                    , HSA.cy 0.0
                    , HE.onClick $ const RequestToResetZoom
                    ]
                ]
            else HH.text ""
        ]
    where
        zoomTextWidth = 35.0
        slopeFactor = 5.0
        fontSize = 9.0


render HTML state =
    HH.div
        []
        [ HH.div
            [ HHP.style $ HHP.position_ HHP.Rel { x : state.width * 0.3 + 5.0, y : 5.0 } <> " " <> HHP.fontSize_ fontSize ]
            [ WF.renderFormatting HTML state.content ]
        ]
    where
        fontSize = 9.0


handleAction :: forall m. Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
    Receive { content, width, currentZoom } -> H.put { content, width, currentZoom }
    RequestToResetZoom -> H.raise ResetZoom


handleQuery :: forall action output m a. Query a -> H.HalogenM State action () output m (Maybe a)
handleQuery = case _ of
    UpdateContent content a -> do
        H.modify_ _ { content = content }
        pure $ Just a