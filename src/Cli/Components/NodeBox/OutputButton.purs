module Cli.Components.NodeBox.OutputButton where

import Prelude


import Control.Monad.State as State

import Effect (Effect)
import Effect.Class (liftEffect)

import Type.Proxy (Proxy)
import Data.Repr (class FromRepr, class ToRepr)
import Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))
import Data.Mark (mark)
import Data.SProxy (reflect)

import Signal (Signal)
import Signal (get) as Signal

import Blessed as B

import Blessed ((>~))
import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Internal.Core as Core
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Tagger as T

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method (setContent) as Box

import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen

import Cli.Keys (NodeBoxKey, OutputsBoxKey, OutputButtonKey, InfoBoxKey, mainScreen, statusLine, outputIndicator)
import Cli.Style as Style
import Cli.State (State)
import Cli.Palette.Set.X11 as X11
import Cli.Palette.Item (crepr) as C
import Cli.Palette as Palette
import Cli.Tagging as T
import Cli.Bounds (collect, outputPos) as Bounds
import Cli.Components.OutputIndicator as OI
import Cli.Components.NodeBox.InfoBox as IB
import Cli.Components.StatusLine as SL
import Cli.Components.FullInfoBox as FI

import Noodle.Id as Id
import Noodle.Node2 (Node) as Noodle
import Noodle.Node2 as Node
import Noodle.Patch4 as Patch

import Toolkit.Hydra2.Repr.Wrap (WrapRepr) as Hydra
import Toolkit.Hydra2.Repr.Info (short, full) as Info


width :: Dimension
width = Dimension.px widthN


widthN :: Int
widthN = 3


left :: Int -> Offset
left idx = Offset.px $ idx * (widthN + 1)


component
    :: forall f nstate o dout is os os'
     . IsSymbol f
    => Id.HasOutput o dout os' os
    => ToRepr dout Hydra.WrapRepr
    => FromRepr Hydra.WrapRepr dout
    => OutputButtonKey
    -> InfoBoxKey
    -> Patch.HoldsNode Effect
    -> NodeBoxKey
    -> OutputsBoxKey
    -> Int
    -> Maybe Hydra.WrapRepr
    -> Signal (Maybe Hydra.WrapRepr)
    -> Proxy dout
    -> Noodle.Node f nstate is os Effect
    -> Id.Output o
    -> Core.Blessed State
component buttonKey nextInfoBox nodeHolder nextNodeBox nextOutputsBox idx maybeRepr reprSignal pdout onode outputId =
    B.button buttonKey
        [ Box.content $ T.render $ T.output idx outputId maybeRepr
        , Box.top $ Offset.px 0
        , Box.left $ left idx
        -- , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width width
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Button.mouse true
        , Style.inputsOutputs
        , Core.on Button.Press
            \_ _ -> onPress buttonKey nodeHolder nextNodeBox idx pdout onode outputId
        , Core.on Element.MouseOver
            $ onMouseOver (Node.family onode) nextNodeBox nextInfoBox idx outputId maybeRepr reprSignal
        , Core.on Element.MouseOut
            $ onMouseOut nextInfoBox idx
        ]
        []


onPress
    :: forall f nstate o dout is os os'
     . IsSymbol f
    => Id.HasOutput o dout os' os
    => ToRepr dout Hydra.WrapRepr
    => FromRepr Hydra.WrapRepr dout
    => OutputButtonKey
    -> Patch.HoldsNode Effect
    -> NodeBoxKey
    -> Int
    -> Proxy dout
    -> Noodle.Node f nstate is os Effect
    -> Id.Output o
    -> BlessedOp State Effect
    -- -> String /\ Array C.Key /\ Core.HandlerFn ListBar "node-outputs-bar" State
onPress buttonKey nodeHolder nextNodeBox index pdout node output =
    {- Id.reflect output /\ [] /\ \_ _ -> -} do
        -- liftEffect $ Console.log $ "press" <> show index
        -- TODO: highlight value
        -- currentContent <- Box.getContent buttonKey
        -- buttonKey >~ Box.setContent "x"
    nodeBounds <- Bounds.collect nextNodeBox
    let outputPos = Bounds.outputPos nodeBounds index
    OI.move { x : outputPos.x, y : outputPos.y - 1 }
    OI.updateStatus OI.WaitConnection
    mainScreen >~ Screen.render
    State.modify_
        (_
            { lastClickedOutput =
                Just
                    { index, subj : reflect output, nodeKey : nextNodeBox
                    , nodeId : Id.holdNodeId (Node.id node), node : nodeHolder
                    , outputId : Node.holdOutputInNodeMRepr pdout node output }
                    }
        )



onMouseOver :: forall o f. IsSymbol o => IsSymbol f => Id.Family' f -> NodeBoxKey -> InfoBoxKey -> Int -> Id.Output o -> Maybe Hydra.WrapRepr -> Signal (Maybe Hydra.WrapRepr) -> _ -> _ -> BlessedOp State Effect
onMouseOver family nodeBox infoBox idx outputId _ reprSignal _ _ = do
    state <- State.get
    nodeBounds <- Bounds.collect nodeBox
    let outputPos = Bounds.outputPos nodeBounds idx
    maybeRepr <- liftEffect $ Signal.get reprSignal
    -- infoBox >~ Box.setContent $ show idx <> " " <> reflect outputId
    infoBox >~ IB.outputInfo outputId
    SL.outputStatus family idx outputId maybeRepr
    FI.outputStatus family idx outputId maybeRepr
    case state.lastClickedOutput of
        Just _ -> pure unit
        Nothing -> do
            OI.move { x : outputPos.x, y : outputPos.y - 1 }
            OI.updateStatus OI.Hover
    mainScreen >~ Screen.render
    --liftEffect $ Console.log $ "over" <> show idx


onMouseOut :: InfoBoxKey -> Int -> _ -> _ -> BlessedOp State Effect
onMouseOut infoBox idx _ _ = do
    state <- State.get
    infoBox >~ IB.clear
    SL.clear
    FI.clear
    case state.lastClickedOutput of
        Just _ -> pure unit
        Nothing -> OI.hide
    mainScreen >~ Screen.render
    --liftEffect $ Console.log $ "out" <> show idx