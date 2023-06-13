module Cli.Components.NodeBox.OutletButton where

import Prelude


import Control.Monad.State as State

import Effect (Effect)
import Type.Proxy (Proxy)
import Data.Repr (class FromRepr, class ToRepr)
import Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))
import Data.Mark (mark)
import Data.SProxy (reflect)

import Blessed as B

import Blessed ((>~))
import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Tagger as T

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method (setContent) as Box
import Blessed.Internal.Core as Core
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen

import Cli.Keys (NodeBoxKey, OutletsBoxKey, OutletButtonKey, InfoBoxKey, mainScreen, statusLine)
import Cli.Style as Style
import Cli.State (State)

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


content ::forall o. IsSymbol o => Int -> Id.Output o -> Maybe Hydra.WrapRepr -> String
content idx outputId = content' idx $ Id.outputR outputId


content' :: Int -> Id.OutputR -> Maybe Hydra.WrapRepr -> String
content' idx outputId (Just repr) =
    T.render $ T.fgcs (mark repr) $ Info.short repr -- "⋱" <> show idx <> "⋰" <> Info.short repr
    -- Info.short repr -- "⋰" <> show idx <> "⋱" <> Info.short repr
content' idx outputId Nothing = "⋰" <> show idx <> "⋱"


slContent ::forall o. IsSymbol o => Int -> Id.Output o -> Maybe Hydra.WrapRepr -> String
slContent idx outputId = slContent' idx $ Id.outputR outputId


slContent' :: Int -> Id.OutputR -> Maybe Hydra.WrapRepr -> String
slContent' idx outputId (Just repr) =
    T.render $ T.fgcs (mark repr) $ Info.full repr -- "⋱" <> show idx <> "⋰" <> Info.short repr
    -- Info.short repr -- "⋰" <> show idx <> "⋱" <> Info.short repr
slContent' idx outputId Nothing = "⋰" <> show idx <> "⋱"


component
    :: forall f nstate o dout is os os'
     . IsSymbol f
    => Id.HasOutput o dout os' os
    => ToRepr dout Hydra.WrapRepr
    => FromRepr Hydra.WrapRepr dout
    => OutletButtonKey
    -> InfoBoxKey
    -> Patch.HoldsNode Effect
    -> NodeBoxKey
    -> OutletsBoxKey
    -> Int
    -> Maybe Hydra.WrapRepr
    -> Proxy dout
    -> Noodle.Node f nstate is os Effect
    -> Id.Output o
    -> Core.Blessed State
component buttonKey nextInfoBox nodeHolder nextNodeBox nextOutletsBox idx maybeRepr pdout node outputId =
    B.button buttonKey
        [ Box.content $ content idx outputId maybeRepr
        , Box.top $ Offset.px 0
        , Box.left $ left idx
        -- , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width width
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Button.mouse true
        , Style.inletsOutlets
        , Core.on Button.Press
            \_ _ -> onPress nodeHolder nextNodeBox idx pdout node outputId
        , Core.on Element.MouseOver
            $ onMouseOver nextInfoBox idx outputId maybeRepr
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
    => Patch.HoldsNode Effect
    -> NodeBoxKey
    -> Int
    -> Proxy dout
    -> Noodle.Node f nstate is os Effect
    -> Id.Output o
    -> BlessedOp State Effect
    -- -> String /\ Array C.Key /\ Core.HandlerFn ListBar "node-outlets-bar" State
onPress nodeHolder nextNodeBox index pdout node output =
    {- Id.reflect output /\ [] /\ \_ _ -> -} do
        -- liftEffect $ Console.log $ "press" <> show index
        State.modify_
            (_
                { lastClickedOutlet =
                    Just
                        { index, subj : reflect output, nodeKey : nextNodeBox
                        , nodeId : Id.holdNodeId (Node.id node), node : nodeHolder
                        , outputId : Node.holdOutputInNodeMRepr pdout node output }
                        }
            )



onMouseOver :: forall o. IsSymbol o => InfoBoxKey -> Int -> Id.Output o -> Maybe Hydra.WrapRepr -> _ -> _ -> BlessedOp State Effect
onMouseOver infoBox idx outputId maybeRepr _ _ = do
    infoBox >~ Box.setContent $ show idx
    statusLine >~ Box.setContent $ slContent idx outputId maybeRepr
    mainScreen >~ Screen.render
    --liftEffect $ Console.log $ "over" <> show idx


onMouseOut :: InfoBoxKey -> Int -> _ -> _ -> BlessedOp State Effect
onMouseOut infoBox idx _ _ = do
    infoBox >~ Box.setContent ""
    mainScreen >~ Screen.render
    --liftEffect $ Console.log $ "out" <> show idx