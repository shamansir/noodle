module Web.Components.StatusBar where

import Prelude


import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
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
    }


type State =
    { content :: T.Tag
    , width :: Number
    }


data Action
    = Receive Input


type Output
    = Unit


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
initialState { content, width } = { content, width }


render :: forall m. TargetLayer -> State -> H.ComponentHTML Action () m
render SVG state =
    HS.g
        []
        [ HS.path
            [ HSA.d $ P.statusBar { slope : slopeFactor, height, width : state.width }
            , HSA.fill $ Just $ P.hColorOf $ _.i800 Palette.yellow
            , HSA.stroke $ Just $ P.hColorOf $ _.i800 Palette.yellow
            ]
        , WF.renderFormatting state.content
        ]
    where
        slopeFactor = 5.0


render HTML state =
    HH.div
        []
        [ HH.text "Status Bar" ]


handleAction :: forall m. Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
    Receive { content, width } -> H.put { content, width }


handleQuery :: forall action output m a. Query a -> H.HalogenM State action () output m (Maybe a)
handleQuery = case _ of
    UpdateContent content a -> do
        H.modify_ _ { content = content }
        pure $ Just a