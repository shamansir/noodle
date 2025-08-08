module Web.Components.StatusBar where

import Prelude

import Prim.Row (class Cons) as Row
import Type.Proxy (Proxy(..))

import Data.Const (Const)
import Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))
import Data.String (take) as String
import Data.Tuple.Nested ((/\), type (/\))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Array (length) as Array
import Data.Int (toNumber) as Int

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

import Front.Shared.StatusBarCells as Cells
import Front.Shared.WsLocation as WS

import Web.Components.StatusBar.ZoomCell as Cell.Zoom
import Web.Components.StatusBar.WSStatusCell as Cell.WSStatus
import Web.Components.SidePanel.WebSocketStatus as WS


type Slots =
    ( cell :: forall q. H.Slot q Cells.Output (TargetLayer /\ Cells.Which)
    )


type CellSlot = H.Slot (Const Void) Cells.Output (TargetLayer /\ Cells.Which)


_cell = Proxy :: _ "cell"


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


data Output
    = Output


data Query a
    = UpdateContent T.Tag a


type CellState r =
    { currentZoom :: Number
    , wsStatus :: WS.Status
    | r
    }


height = 25.0 :: Number


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
initialState { content, width  }
    = { content, width }


render :: forall m. TargetLayer -> State -> H.ComponentHTML Action Slots m
render SVG state =
    HS.g
        [] $
        [ HS.path
            [ HSA.d $ P.statusBar { slope : slopeFactor, height, width : state.width }
            , HSA.fill $ Just $ P.hColorOf $ _.i950 Palette.yellow
            , HSA.stroke $ Just $ P.hColorOf $ _.i800 Palette.yellow
            ]
        ]
        -- <> (mapWithIndex wrapSvgWithPos $ cellSlot cellWidth SVG state <$> Cells.allCells)
        {-
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
        -}
    where
        zoomTextWidth = 35.0
        slopeFactor = 5.0
        cellsX = state.width - cellsWidth
        cellsY = height / 2.0 -- center the text
        cellsWidth = (state.width * 0.3)
        cellsCount = Array.length Cells.allCells
        cellWidth = cellsWidth / Int.toNumber cellsCount
        wrapSvgWithPos cellIdx content =
            HS.g
                [ HSA.transform [ HSA.Translate (cellsX + Int.toNumber cellIdx * cellWidth {- FIXME -}) cellsY ]
                ]
                $ pure content


render HTML state =
    HH.div
        [ HHP.style "position: relative;" ] $
        [ HH.div
            [ HHP.style $ HHP.position_ HHP.Rel { x : state.width * 0.3 + 5.0, y : 5.0 } <> " " <> HHP.fontSize_ fontSize ]
            [ WF.renderFormatting HTML state.content ]
        ]
        -- <> (mapWithIndex wrapHtmlWithPos $ cellSlot cellWidth HTML state <$> Cells.allCells)
    where
        cellsX = state.width - cellsWidth
        cellsY = height / 2.0 -- center the text
        cellsWidth = (state.width * 0.3)
        cellsCount = Array.length Cells.allCells
        cellWidth = cellsWidth / Int.toNumber cellsCount
        wrapHtmlWithPos cellIdx content =
            HH.div
                [ HHP.style $
                    HHP.position_ HHP.Rel
                        { x : cellsX + Int.toNumber cellIdx * cellWidth
                        , y : cellsY
                        }
                    <> " min-width: " <> show cellWidth <> "px;"
                ]
                $ pure content


handleAction :: forall m. Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
    Receive { content, width } -> H.put { content, width }


handleQuery :: forall action output m a. Query a -> H.HalogenM State action Slots output m (Maybe a)
handleQuery = case _ of
    UpdateContent content a -> do
        H.modify_ _ { content = content }
        pure $ Just a


fontSize = 9.0 :: Number


cellSlot
    :: forall m slot action slots _1 r
     . Row.Cons slot CellSlot _1 slots
    => IsSymbol slot
    => Proxy slot
    -> TargetLayer
    -> (Cells.Output -> action)
    -> CellState r
    -> Cells.Which
    -> H.ComponentHTML action slots m
cellSlot pslot target toAction state =
    case _ of
        Cells.Zoom     -> HH.slot pslot (target /\ Cells.Zoom)     (Cell.Zoom.component target)     { currentZoom : state.currentZoom, fontSize }               toAction
        Cells.WSStatus -> HH.slot pslot (target /\ Cells.WSStatus) (Cell.WSStatus.component target) { host : WS.host, port : WS.port, status : state.wsStatus } toAction


cellWidth :: Cells.Which -> Number
cellWidth =
    case _ of
      Cells.WSStatus -> 250.0
      Cells.Zoom -> 50.0


contentMinWidth = 400.0 :: Number
