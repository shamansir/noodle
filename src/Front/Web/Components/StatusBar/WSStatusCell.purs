module Web.Components.StatusBar.WSStatusCell where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.String (take) as String
import Data.Tuple.Nested ((/\), type (/\))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS

import Noodle.Ui.Palette.Item as P
import Noodle.Ui.Palette.Set.Flexoki as Palette

import Data.Text.Format (Tag) as T

import Web.Layer (TargetLayer(..))

import Front.Shared.StatusBarCells as Cells
import Web.Components.SidePanel.WebSocketStatus as WS
import WebSocket.Types as WS


type Input =
    { host :: WS.Host
    , port :: WS.Port
    , status :: WS.Status
    }


type State =
    { host :: WS.Host
    , port :: WS.Port
    , status :: WS.Status
    }


data Action
    = Initialize
    | Receive Input


component :: forall query m. TargetLayer -> H.Component query Input Cells.Output m
component targetLayer =
    H.mkComponent
        { initialState : initialState
        , render : render targetLayer
        , eval : H.mkEval H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            , receive = Just <<< Receive
            }
        }

    where
    initialState { host, port, status } =
        { host, port, status }

    render SVG state =
        HS.g
            []
            [ HS.text
                [ HSA.fill $ Just $ P.hColorOf $ _.i100 Palette.green
                , HSA.font_size $ HSA.FontSizeLength $ HSA.Px 9.0
                , HSA.dominant_baseline HSA.Central
                , HSA.transform [ HSA.Translate 0.0 11.0 ]
                ]
                [ HH.text $ fullText state ]
            ]

    render HTML state =
        HH.span [] []
        -- HH.span [ {- HHP.style "postion: absolute;" -} ] [ HH.text "HTML:TEST" ]

    fullText state = serverNameText state.host state.port <> " :: " <> statusText state.status

    serverNameText (WS.Host host) (WS.Port port) =
        host <> ":" <> show port

    statusText =
        case _ of
            WS.Off -> "No connection"
            WS.Waiting -> "Waiting"
            WS.Connected mbHash { total } ->
                "Connected: " <> case mbHash of
                    Just uHash -> show uHash
                    Nothing -> "-"
            WS.Error -> "Error"

    handleAction = case _ of
        Initialize ->
            pure unit
        Receive { host, port, status } ->
            H.put { host, port, status }