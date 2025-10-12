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
import Web.Components.StatusBar.UiModeCell as Cell.UiMode
import Web.Components.StatusBar.KeyboardComboCell as Cell.KeyboardCombo
import Web.Components.SidePanel.WebSocketStatus as WS
import Web.Components.AppScreen.UiMode (UiMode)
import Web.Components.AppScreen.UiMode as UiMode
import Web.Components.AppScreen.KeyboardLogic as KL

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
    , uiMode :: UiMode
    , keyboardFocus :: KL.Focus
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
    where
        slopeFactor = 5.0


render HTML state =
    HH.div
        [ HHP.style $ "position: relative;" ] $
        [ HH.div
            [ HHP.style
                 $ HHP.position_ HHP.Rel { x : state.width * 0.3 + 5.0, y : 5.0 } <> " "
                <> HHP.fontSize_ fontSize
                <> " width : " <> show state.width <> "px; "
                <> "overflow-x: hidden; "
                ]
            [ WF.renderFormatting HTML state.content ]
        ]



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
    -> { width :: Number, height :: Number }
    -> (Cells.Output -> action)
    -> CellState r
    -> Cells.Which
    -> H.ComponentHTML action slots m
cellSlot pslot target size toAction state =
    {- HS.g
        []
        [ HS.rect
            [ HSA.width size.width
            , HSA.height $ size.height - 10.0
            , HSA.fill $ Just $ P.hColorOf $ _.i950 Palette.yellow
            ] -}
    case _ of
        Cells.Zoom          -> HH.slot pslot (target /\ Cells.Zoom)          (Cell.Zoom.component target)          { currentZoom : state.currentZoom, fontSize }               toAction
        Cells.WSStatus      -> HH.slot pslot (target /\ Cells.WSStatus)      (Cell.WSStatus.component target)      { host : WS.host, port : WS.port, status : state.wsStatus } toAction
        Cells.UiMode        -> HH.slot pslot (target /\ Cells.UiMode)        (Cell.UiMode.component target)        { currentMode : UiMode.getModeKey state.uiMode, fontSize }  toAction
        Cells.KeyboardCombo -> HH.slot pslot (target /\ Cells.KeyboardCombo) (Cell.KeyboardCombo.component target) state.keyboardFocus                                         toAction


cellWidth :: Cells.Which -> Number
cellWidth =
    case _ of
      Cells.WSStatus -> 270.0
      Cells.Zoom -> 50.0
      Cells.UiMode -> 50.0
      Cells.KeyboardCombo -> 100.0


contentMinWidth = 400.0 :: Number
