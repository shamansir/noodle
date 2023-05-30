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

import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Key (Key) as C
import Blessed.Core.Offset (Offset)
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

import Cli.Keys (NodeBoxKey, OutletsBoxKey, OutletButtonKey)
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
content' idx outputId (Just repr) = Info.short repr -- "⋰" <> show idx <> "⋱" <> Info.short repr
content' idx outputId Nothing = "⋰" <> show idx <> "⋱"


component
    :: forall f nstate o dout is os os'
     . IsSymbol f
    => Id.HasOutput o dout os' os
    => ToRepr dout Hydra.WrapRepr
    => FromRepr Hydra.WrapRepr dout
    => OutletButtonKey
    -> Patch.HoldsNode Effect
    -> NodeBoxKey
    -> OutletsBoxKey
    -> Int
    -> Maybe Hydra.WrapRepr
    -> Proxy dout
    -> Noodle.Node f nstate is os Effect
    -> Id.Output o
    -> Core.Blessed State
component buttonKey nodeHolder nextNodeBox nextOutletsBox idx maybeRepr pdout node outputId =
    B.button buttonKey
        [ Box.content $ content idx outputId maybeRepr
        , Box.top $ Offset.px 0
        , Box.left $ left idx
        -- , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width width
        , Box.height $ Dimension.px 1
        , Button.mouse true
        , Style.inletsOutlets
        , Core.on Button.Press
            \_ _ -> onPress nodeHolder nextNodeBox idx pdout node outputId
        -- , Core.on Element.MouseOver
        --     $ onMouseOver idx
        -- , Core.on Element.MouseOut
        --     \_ _ -> onMouseOut idx
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
                        { index, subj : Id.reflect output, nodeKey : nextNodeBox
                        , nodeId : Id.holdNodeId (Node.id node), node : nodeHolder
                        , outputId : Node.holdOutputInNodeMRepr pdout node output }
                        }
            )



onMouseOver :: Int -> _ -> _ -> BlessedOp State Effect
onMouseOver idx _ _ =
    liftEffect $ Console.log $ "over" <> show idx


onMouseOut :: Int -> BlessedOp State Effect
onMouseOut idx =
    liftEffect $ Console.log $ "out" <> show idx