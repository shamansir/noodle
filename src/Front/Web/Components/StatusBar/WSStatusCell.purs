module Web.Components.StatusBar.WSStatusCell where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.String (take) as String
import Data.Tuple.Nested ((/\), type (/\))

import Halogen as H
import Halogen.HTML as HH
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

    zoomTextWidth = 35.0

    render SVG state =
        HS.g [] []

    render HTML state =
        HH.span [] []

    handleAction = case _ of
        Initialize ->
            pure unit
        Receive { host, port, status } ->
            H.put { host, port, status }