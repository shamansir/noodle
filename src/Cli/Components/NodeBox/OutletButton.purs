module Cli.Components.NodeBox.OutletButton where

import Prelude


import Control.Monad.State as State
import Effect.Class (liftEffect)
import Effect.Console (log) as Console

import Effect (Effect)
import Type.Proxy (Proxy)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Repr (class FromRepr, class ToRepr)
import Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))

import Blessed as B
import Cli.Keys as Key

import Blessed.Core.Dimension as Dimension
import Blessed.Core.Key (Key) as C
import Blessed.Core.Offset as Offset
import Blessed.Internal.BlessedOp (BlessedOp)

import Blessed.Internal.Core (Blessed) as C
import Blessed.Internal.BlessedSubj (ListBar)

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Lists.List.Option (keys, mouse) as List
import Blessed.UI.Lists.ListBar.Option (commands) as ListBar
import Blessed.Internal.Core as Core
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element

import Cli.Keys (NodeBoxKey, OutletsBoxKey)
import Cli.Style as Style
import Cli.State (State)

import Noodle.Id as Id
import Noodle.Node2 (Node) as Noodle
import Noodle.Node2 as Node
import Noodle.Patch4 as Patch

import Toolkit.Hydra2.Repr.Wrap (WrapRepr) as Hydra


component
    :: forall f nstate o dout is os os'
     . IsSymbol f
    => Id.HasOutput o dout os' os
    => ToRepr dout Hydra.WrapRepr
    => FromRepr Hydra.WrapRepr dout
    => Patch.HoldsNode Effect
    -> NodeBoxKey
    -> OutletsBoxKey
    -> Int
    -> Proxy dout
    -> Noodle.Node f nstate is os Effect
    -> Id.Output o
    -> Core.Blessed State
component nodeHolder nextNodeBox nextOutletsBox idx pdout node outputId =
    B.button Key.outletButton
        [ Box.content "+"
        , Box.top $ Offset.px 0
        , Box.left $ Offset.px $ idx * 2
        -- , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Button.mouse true
        , Style.addPatch
        , Core.on Button.Press
            \_ _ -> onPress nodeHolder nextNodeBox idx pdout node outputId
        , Core.on Element.MouseOver
            $ onMouseOver idx
        , Core.on Element.MouseOut
            \_ _ -> onMouseOut idx
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
        -- liftEffect $ Console.log $ "handler " <> oname
        State.modify_
            (_
                { lastClickedOutlet =
                    Just
                        { index, subj : Id.reflect output, nodeKey : nextNodeBox, nodeId : Id.holdNodeId (Node.id node), outputId : Node.holdOutputInNodeMRepr pdout node output, node : nodeHolder } })


onMouseOver :: Int -> _ -> _ -> BlessedOp State Effect
onMouseOver idx _ _ =
    liftEffect $ Console.log $ "over" <> show idx


onMouseOut :: Int -> BlessedOp State Effect
onMouseOut idx =
    liftEffect $ Console.log $ "out" <> show idx